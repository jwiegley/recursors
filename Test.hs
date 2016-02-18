{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Test where

import Final
import Control.Lens
import Control.Lens.Iso
import Data.Function

data Foo a = A Int
           | B Int Float
           | C a (Foo a)

$(makeFinalIso ''Test.Foo)

-- toFooR f = FooR (\a b c -> case f of
--                          A x -> a x
--                          B x y -> b x y
--                          C x y -> c x (foldFooR (toFooR y) a b c))

-- fooIso :: Iso' (Foo a) (FooR a)
-- fooIso = iso (fix $ \k f ->
--                   FooR (\a b c -> case f of
--                                 A x -> a x
--                                 B x y -> b x y
--                                 C x y -> c x (foldFooR (k y) a b c)))
--              (\f -> foldFooR f A B C)
