{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import           Data.Functor (void)

import           Control.Lens

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Hedgehog.Checkers.Ugly.Function.Hack

import Hedgehog.Checkers.Lens.Properties

data Foo =
  Foo
  { _bar :: Int
  , _baz :: Int
  , _dunno :: String }
  deriving (Eq, Show)

makeLenses ''Foo

genFoo' :: Gen Int -> Gen String -> Gen Foo
genFoo' gi gs = do
  i <- gi
  i' <- gi
  s <- gs
  return (Foo i i' s)

genFoo :: Gen Foo
genFoo =
  let string = Gen.string (Range.linear 0 100) Gen.ascii
      int = Gen.int (Range.linear 0 100)
  in genFoo' int string

allLensLawsFoo :: Property
allLensLawsFoo = property $ do
  let string = Gen.string (Range.linear 0 100) Gen.ascii
      int = Gen.int (Range.linear 0 100)
  isLens bar int genFoo (ordFuncWtf' int int)
  isLens baz int genFoo (ordFuncWtf' int int)
  isLens dunno string genFoo (ordFuncWtf' string string)

main :: IO ()
main = do
  void $
    checkParallel $
      Group "Control.Lens.Lens" [ ("all laws applied to foo's lenses"
                                  , allLensLawsFoo)
                                ]
