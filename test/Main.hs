{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE EmptyCase #-}

module Main where

import Control.Final
-- import Control.Lens

data Type1
$(makeFinalIso ''Type1)

data Type2 = Type2
$(makeFinalIso ''Type2)

data Type3 = Type3 Int
$(makeFinalIso ''Type3)
data Type3b = forall m. Monad m => Type3b (m Int)
$(makeFinalIso ''Type3b)

data Type4 = Type4 Int Char
$(makeFinalIso ''Type4)
data Type4b = Type4b { foo5 :: Int, bar5 :: Char }
$(makeFinalIso ''Type4b)

data Foo a = A Int
           | B Int Float
           | C a (Foo a)

$(makeFinalIso ''Foo)

{-
newtype FooR a_ahEX
  = FooR {foldFooR :: forall r.
                      (Int -> r) -> (Int -> Float -> r) -> (a_ahEX -> r -> r) -> r}
toFooR :: forall a_ahEX. Foo a_ahEX -> FooR a_ahEX
toFooR x
  = FooR
      (\ a_ahFq b_ahFr c_ahFs
         -> case x of {
              A a_ahFt -> a_ahFq a_ahFt
              B a_ahFu a_ahFv -> b_ahFr a_ahFu a_ahFv
              C a_ahFw a_ahFx
                -> c_ahFs a_ahFw (foldFooR (toFooR a_ahFx) a_ahFq b_ahFr c_ahFs) })
fromFooR :: forall a_ahEX. FooR a_ahEX -> Foo a_ahEX
fromFooR x = foldFooR x A B C
isoFooR :: forall a_ahEX. Iso' (Foo a_ahEX) (FooR a_ahEX)
isoFooR = iso toFooR fromFooR
-}

main :: IO ()
main = putStrLn "Hello, world!"
