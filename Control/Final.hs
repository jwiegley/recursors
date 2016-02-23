{-# LANGUAGE LambdaCase #-}

module Control.Final where

import Language.Haskell.TH
import Control.Monad
import Data.Char (toLower)
import Data.List (foldl')
import Data.Foldable (foldr')

-- Substitute any occurrence of 'typ' with 'repl' throughout the given type.
substituteType :: Type -> Type -> Type -> Type
substituteType typ repl = go where
    go s | s == typ  = repl
         | otherwise = case s of
             ForallT bs c t -> ForallT bs c (go t)
             AppT t1 t2     -> AppT (go t1) (go t2)
             SigT t k       -> SigT (go t) k
             _              -> s

-- Turn r and [a,b,c] into: a -> b -> c -> r
funType :: Type -> [Type] -> Type
funType = foldr' (AppT . AppT ArrowT)

-- Turn f and [a,b,c] into: f a b c
funCall :: Exp -> [Exp] -> Exp
funCall = foldl' AppE

makeFinalIso :: String -> Name -> DecsQ
makeFinalIso dest = reify >=> \case
    TyConI (DataD ctx name binders ctors _) ->
        makeFinalType dest ctx name (map simplifyBinder binders) ctors
    TyConI (NewtypeD ctx name binders ctor _) ->
        makeFinalType dest ctx name (map simplifyBinder binders) [ctor]
    _ -> error "makeFinalIso only accepts plain ADTs (data or newtype)"
  where
    simplifyBinder (KindedTV b StarT) = PlainTV b
    simplifyBinder b = b

makeFinalType :: String -> Cxt -> Name -> [TyVarBndr] -> [Con] -> DecsQ
makeFinalType dest ctx name binders ctors = do
    -- The final encoding is named StreamR
    -- The final unwrapper is foldStreamR
    -- The final type is 'forall r. (a -> r -> r) -> r'
    -- newtype StreamR a = StreamR { foldStreamR :: 'foldType' }
    let nameR     = mkName dest
        foldNameR = mkName ("fold" ++ dest)
        nameR'    = mkName dest
        newType   =
            NewtypeD ctx nameR binders
                     (RecC nameR' [(foldNameR, NotStrict, foldType)]) []

    -- toStreamR :: Stream a -> StreamR a
    let nameToNameR = mkName ("to" ++ dest)
    funs    <- ctorFuns
    matches <- ctorMatches funs foldNameR nameToNameR
    let xname   = mkName "x"
        lam     = LamE (map VarP funs) (CaseE (VarE xname) matches)
        toBody  = NormalB (AppE (ConE nameR) lam)
        toNameR =
            [ SigD nameToNameR (funSig ArrowT name nameR)
            , FunD nameToNameR [Clause [VarP xname] toBody []] ]

    -- fromStreamR :: StreamR a -> Stream a
    let fromBody = NormalB (funCall (VarE foldNameR)
                                    (VarE xname : map (ConE . ctorName) ctors))
        nameFromNameR = mkName ("from" ++ dest)
        fromNameR =
            [ SigD nameFromNameR (funSig ArrowT nameR name)
            , FunD nameFromNameR [Clause [VarP xname] fromBody []] ]

    -- If Lens is imported, make isoStreamR :: Iso' (Stream a) (StreamR a)
    miso    <- lookupValueName "Control.Lens.iso"
    misoTyp <- lookupTypeName  "Control.Lens.Iso'"
    let isoStreamR = mkName ("iso" ++ dest)
        lensIsoBody iso =
            NormalB (AppE (AppE (VarE iso) (VarE nameToNameR))
                          (VarE nameFromNameR))
        lensIso = case (miso, misoTyp) of
            (Just iso, Just iso') ->
                [ SigD isoStreamR (funSig (ConT iso') name nameR)
                , FunD isoStreamR [Clause [] (lensIsoBody iso) []]
                ]
            _ -> []

    return $ [newType] ++ toNameR ++ fromNameR ++ lensIso
  where
    -- The foldType is the Church-encoding of the input ADT
    foldType =
        let r  = mkName "r"
            rt = VarT r in
        substituteType nameBaseType rt
            $ ForallT [PlainTV r] []
            $ funType rt
            $ map (ctorToFunc rt) ctors
      where
        ctorToFunc r (NormalC _ ts)     = funType r (map snd ts)
        ctorToFunc r (RecC _ ts)        = funType r (map (\(_,_,x) -> x) ts)
        ctorToFunc r (InfixC t1 _ t2)   = funType r [snd t1, snd t2]
        ctorToFunc r (ForallC bs ct co) = ForallT bs ct (ctorToFunc r co)

    -- Return just the name of the constructor
    ctorName (NormalC n _)    = n
    ctorName (RecC n _)       = n
    ctorName (InfixC _ n _)   = n
    ctorName (ForallC _ _ co) = ctorName co

    -- Return a list of function names based on our constructor names, where
    -- FooBar becomes fooBar, etc.
    ctorFuns = mapM (newName . (\n -> toLower (head n) : tail n)
                             . nameBase . ctorName) ctors

    -- Return a "case of" construction that matches against the input ADT
    ctorMatches funs foldNameR nameToNameR =
        forM (zip funs ctors) $ \(f, c) -> do
            let poss = ctorArgCount c
            args <- replicateM (length poss) (newName "a")
            return $
                Match (ConP (ctorName c) (map VarP args))
                      (NormalB (foldl' app (VarE f)
                                       (zip (map VarE args) poss))) []
      where
        ctorArgCount (NormalC _ ts)   = map (\(_,t)   -> t == nameBaseType) ts
        ctorArgCount (RecC _ ts)      = map (\(_,_,t) -> t == nameBaseType) ts
        ctorArgCount (InfixC t1 _ t2) = [snd t1 == nameBaseType,
                                         snd t2 == nameBaseType]
        ctorArgCount (ForallC _ _ co) = ctorArgCount co

        app acc (arg, recurse) =
            AppE acc $ if recurse
                       then funCall (AppE (VarE foldNameR)
                                          (AppE (VarE nameToNameR) arg))
                                    (map VarE funs)
                       else arg

    -- Return a fully applied "base type" using the structure of the input
    -- ADT. For example "<name> a b c", if the ADT takes three type variables.
    baseType nm =
        foldl' (\acc -> AppT acc . VarT) (ConT nm)
            $ map (\case PlainTV n    -> n
                         KindedTV n _ -> n) binders

    -- The original name of the input ADT, fully applied. This is used to
    -- discover recursive occurrences.
    nameBaseType = baseType name

    -- Create a function signature based on the input ADT's binders
    funSig t nF nT =
        let ty = AppT (AppT t (baseType nF)) (baseType nT) in
        if null binders
        then ty
        else ForallT binders ctx ty
