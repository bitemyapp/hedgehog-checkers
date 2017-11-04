{-# LANGUAGE RankNTypes #-}

module Hedgehog.Checkers.Lens
  ( viewOfSetIdentity
  , setOfViewIdentity
  , doubleSetIdentity
  ) where

import           Control.Lens

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

-- type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t

-- 1) You get back what you put in:
-- view l (set l v s)  ≡ v

viewOfSetIdentity :: ( Eq b
                     , Show b
                     , Show s
                     )
                  => Lens' s b
                  -> Gen b
                  -> Gen s
                  -> PropertyT IO ()
viewOfSetIdentity setter genv gens = do
  val <- forAll genv
  settee <- forAll gens
  (view setter (set setter val settee)) === val

-- 2) Putting back what you got doesn't change anything:
-- set l (view l s) s  ≡ s

setOfViewIdentity :: ( Eq s
                     , Show s
                     )
                  => Lens' s b
                  -> Gen s
                  -> PropertyT IO ()
setOfViewIdentity setter gens = do
  settee <- forAll gens
  (set setter (view setter settee) settee) === settee

-- 3) Setting twice is the same as setting once:
-- set l v' (set l v s) ≡ set l v' s

doubleSetIdentity :: ( Eq s
                     , Show b
                     , Show s
                     )
                  => Lens' s b
                  -> Gen b
                  -> Gen s
                  -> PropertyT IO ()
doubleSetIdentity setter genv gens = do
  val <- forAll genv
  val' <- forAll genv
  settee <- forAll gens
  (set setter val' (set setter val settee)) === set setter val' settee
