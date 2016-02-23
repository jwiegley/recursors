{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Final
import Test.Hspec
import Test.QuickCheck
-- import Control.Lens

data Type1
$(makeFinalIso "Type1R" ''Type1)

data Type2 = Type2 deriving (Show, Eq)
$(makeFinalIso "Type2R" ''Type2)

instance Arbitrary Type2 where
    arbitrary = pure Type2

instance Arbitrary Type2R where
    arbitrary = pure $ toType2R Type2

instance Eq Type2R where
    x == y = fromType2R x == fromType2R y
instance Show Type2R where
    show = show . fromType2R

data Type3 = Type3 Int deriving (Show, Eq)
$(makeFinalIso "Type3R" ''Type3)
data Type3b = forall m. Monad m => Type3b (m Int)
$(makeFinalIso "Type3bR" ''Type3b)

data Type4 = Type4 Int Char deriving (Show, Eq)
$(makeFinalIso "Type4R" ''Type4)
data Type4b = Type4b { foo5 :: Int, bar5 :: Char } deriving (Show, Eq)
$(makeFinalIso "Type4bR" ''Type4b)

data Foo a = A Int
           | B Int Float
           | C a (Foo a) deriving (Show, Eq)

$(makeFinalIso "FooR" ''Foo)

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
main = hspec $ parallel $ do
    describe "Type2" $ do
      it "toType2R . fromType2R = id" $ property $
          \x -> toType2R (fromType2R x) == x
      it "fromType2R . toType2R = id" $ property $
          \x -> fromType2R (toType2R x) == x
