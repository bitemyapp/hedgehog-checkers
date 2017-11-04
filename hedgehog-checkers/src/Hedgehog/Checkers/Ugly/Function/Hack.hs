module Hedgehog.Checkers.Ugly.Function.Hack where

import           Data.Map (Map)
import qualified Data.Map as Map

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

---------- vvvvv CANCER PLEASE IGNORE vvvvv  -----------------------------

fromMap :: Ord k => v -> Map k v -> k -> v
fromMap defaultValue kvs k =
  case Map.lookup k kvs of
    Nothing ->
      defaultValue
    Just value ->
      value

ordFuncWtf'' :: Ord a => Range Int -> Gen a -> Gen b -> Gen (a -> b)
ordFuncWtf'' range gen gen' = do
  defaultV <- gen'
  let tupGen = (,) <$> gen <*> gen'
  map <- Gen.map range tupGen
  return $ fromMap defaultV map

ordFuncWtf' :: Ord a => Gen a -> Gen b -> Gen (a -> b)
ordFuncWtf' = ordFuncWtf'' (Range.linear 0 1000)

funcForAllWtf :: Monad m => Gen a -> PropertyT m a
funcForAllWtf g = do
  let funcShow _ = "<func>"
  forAllWith funcShow $ g

ordFuncWtf :: (Ord a, Monad m) => Gen a -> Gen b -> PropertyT m (a -> b)
ordFuncWtf gena genb = do
  -- let funcShow _ = "<func>"
  -- forAllWith funcShow $ ordFuncWtf' gena genb
  funcForAllWtf $ ordFuncWtf' gena genb

---------- ^^^^^ CANCER PLEASE IGNORE ^^^^^  -----------------------------
