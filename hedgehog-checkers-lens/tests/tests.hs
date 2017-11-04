{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import           Data.Functor (void)

import           Control.Lens
import           Control.Lens.TH

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Hedgehog.Checkers.Lens

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

viewOfSet :: Property
viewOfSet = property $ do
  let string = Gen.string (Range.linear 0 100) Gen.ascii
      int = Gen.int (Range.linear 0 100)
  viewOfSetIdentity bar int genFoo
  viewOfSetIdentity baz int genFoo
  viewOfSetIdentity dunno string genFoo

setOfView :: Property
setOfView = property $ do
  setOfViewIdentity bar genFoo
  setOfViewIdentity baz genFoo
  setOfViewIdentity dunno genFoo

doubleSet :: Property
doubleSet = property $ do
  let string = Gen.string (Range.linear 0 100) Gen.ascii
      int = Gen.int (Range.linear 0 100)
  doubleSetIdentity bar int genFoo
  doubleSetIdentity baz int genFoo
  doubleSetIdentity dunno string genFoo

main :: IO ()
main = do
  void $
    checkParallel $
      Group "Control.Lens.Lens" [ ("view l (set l v s)  ≡ v"
                                  , viewOfSet)
                                , ("set l (view l s) s  ≡ s"
                                  , setOfView)
                                , ("set l v' (set l v s) ≡ set l v' s"
                                  , doubleSet)
                                ]
