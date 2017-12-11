{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Hedgehog.Checkers.Classes
  (
  -- | Classes
    ord
  , alt
  , alternative
  , alternativeAltAgreement
  , bifunctor
  , functor
  , semigroup
  , monoid
  , apply
  , applicative
  , applicativeApplyAgreement
  ) where

import           Control.Applicative
import           Data.Bifunctor
import           Data.Functor.Alt
import           Data.Semigroup

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Hedgehog.Checkers.Properties
import Hedgehog.Checkers.Ugly.Function.Hack

-- | Total ordering, genf (a -> Gen a) should
--   always return a value equal to or higher
--   than its input.
ord :: forall a. (Eq a, Ord a, Show a)
    => Gen a -> (a -> Gen a) -> PropertyT IO ()
ord gena genf = do
  reflexive rel gena
  transitive rel gena genf
  antiSymmetric rel gena genf
  where
    rel = (<=)

-- | <!> is associative:             (a <!> b) <!> c = a <!> (b <!> c)
--   <$> left-distributes over <!>:  f <$> (a <!> b) = (f <$> a) <!> (f <$> b)
alt :: ( Alt f
       , Eq (f a)
       , Show (f a)
       )
    => Gen (f a) -> PropertyT IO ()
alt gen = do
  associativity (<!>) gen
-- f <$> (a <!> b) = (f <$> a) <!> (f <$> b)

-- | Alternative instances should respect identity
--   (left and right) and associativity for (<|>)
--   empty <|> x  =  x
--   x <|> empty  =  x
--
--   a <|> (b <|> c)  =  (a <|> b) <|> c
alternative :: ( Alternative f
               , Eq (f a)
               , Show (f a)
               )
            => Gen (f a) -> PropertyT IO ()
alternative gen = do
  identity (<|>) empty gen
  associativity (<|>) gen

alternativeAltAgreement :: ( Alt f
                           , Alternative f
                           , Eq (f a)
                           , Show (f a)
                           )
                        => Gen (f a) -> PropertyT IO ()
alternativeAltAgreement gen = do
  fa <- forAll gen
  fb <- forAll gen
  (fa <!> fb) === (fa <|> fb)

-- fmap (f . g)  ==  fmap f . fmap g
-- ??? inferrable from: fmap id = id
functor :: ( Functor f
           , Eq (f a)
           , Show (f a)
           )
        => Gen (f a) -> PropertyT IO ()
functor gen = do
  functorIdentity
  where functorIdentity = do
          fa <- forAll gen
          fmap id fa === id fa

-- bimap id id ≡ id
-- first id ≡ id
-- second id ≡ id
-- bimap f g ≡ first f . second g
bifunctor :: -- forall f a b .
             ( Bifunctor f
             , Eq (f a b)
             , Eq (f c c)
             , Ord a
             , Ord b
             , Show (f a b)
             , Show (f c c)
             )
          => Gen (f a b)
          -> Gen a
          -> Gen b
          -> Gen c
          -> PropertyT IO ()
bifunctor gen gena genb genc = do
  bimapIdentity
  firstIdentity
  secondIdentity
  bimapFirstSecondDistribute
  where bimapIdentity = do
          fab <- forAll gen
          bimap id id fab === id fab
        firstIdentity = do
          fab <- forAll gen
          first id fab === id fab
        secondIdentity = do
          fab <- forAll gen
          second id fab === id fab
        bimapFirstSecondDistribute = do
          fab <- forAll gen
          f <- ordFuncWtf gena genc
          g <- ordFuncWtf genb genc
          bimap f g fab === (first f . second g) fab

semigroup :: ( Semigroup a
             , Eq a
             , Show a
             )
          => Gen a
          -> PropertyT IO ()
semigroup gen = do
  associativity (<>) gen

monoid :: ( Monoid a
          , Semigroup a
          , Eq a
          , Show a
          )
       => Gen a
       -> PropertyT IO ()
monoid gen = do
  semigroup gen
  identity mappend mempty gen
  associativity mappend gen
  monoidSemigroupSame
  where monoidSemigroupSame = do
          a <- forAll gen
          b <- forAll gen
          mappend a b === a <> b

apply :: forall f a b c
       . ( Apply f
         , Eq (f a)
         , Eq (f b)
         , Eq (f c)
         , Ord a
         , Ord b
         , Show a
         , Show b
         , Show c
         , Show (f a)
         , Show (f b)
         , Show (f c)
         )
      => Gen (f a)
      -> Gen a
      -> Gen b
      -> Gen c
      -> PropertyT IO ()
apply gen gena genb genc = do
  applyComposition
  applyRight
  applyLeft
  where applyComposition = do
          fa <- forAll gen
          fbc <- liftedFunctionWtf gen genb genc
          fab <- liftedFunctionWtf gen gena genb
          ((.) <$> fbc <.> fab <.> fa) === (fbc <.> (fab <.> fa))
        applyRight = do
          fa <- forAll gen
          fbc <- liftedFunctionWtf gen genb genc
          ab <- ordFuncWtf gena genb
          (fbc <.> (ab <$> fa)) === ((. ab) <$> fbc <.> fa)
        applyLeft = do
          fa <- forAll gen
          fab <- liftedFunctionWtf gen gena genb
          bc <- ordFuncWtf genb genc
          (bc <$> (fab <.> fa)) === ((bc .) <$> fab <.> fa)

applicative :: forall f a b c
             . ( Applicative f
               , Eq (f a)
               , Eq (f b)
               , Eq (f c)
               , Ord a
               , Ord b
               , Show a
               , Show (f a)
               , Show (f b)
               , Show (f c)
               )
            => Gen (f a)
            -> Gen a
            -> Gen b
            -> Gen c
            -> PropertyT IO ()
applicative gen gena genb genc = do
  applicativeIdentity
  applicativeComposition
  applicativeHomomorphism
  applicativeInterchange
  applicativeFunctor
  where applicativeIdentity = do
          fa <- forAll gen
          (pure id <*> fa) === fa

        applicativeComposition = do
          fa <- forAll gen
          fbc <- liftedFunctionWtf gen genb genc
          fab <- liftedFunctionWtf gen gena genb
          (pure (.) <*> fbc <*> fab <*> fa) === (fbc <*> (fab <*> fa))

        applicativeHomomorphism = do
          a <- forAll gena
          f <- ordFuncWtf gena genb
          let p :: x -> f x
              p = pure
          (p f <*> p a) === p (f a)

        applicativeInterchange = do
          a <- forAll gena
          fab <- liftedFunctionWtf gen gena genb
          (fab <*> pure a) === (pure ($ a) <*> fab)

        applicativeFunctor = do
          fa <- forAll gen
          f <- ordFuncWtf gena genb
          fmap f fa === (pure f <*> fa)

applicativeApplyAgreement :: ( Monad m
                             , Apply f
                             , Applicative f
                             , Show b
                             , Show (f a)
                             , Show (f b)
                             , Eq (f b)
                             , Ord a
                             )
                          => Gen (f a) -> Gen a -> Gen b -> PropertyT m ()
applicativeApplyAgreement gen gena genb = do
  fa <- forAll gen
  fab <- liftedFunctionWtf gen gena genb
  (fab <.> fa) === (fab <*> fa)

---- Done
-- (Semigroup e, Monoid e) => Alternative (Validation e)	 
-- Alt (Validation e)	 
-- Functor (Validation e)
-- Bifunctor Validation
-- Semigroup e => Semigroup (Validation e a)	 
-- Monoid e => Monoid (Validation e a)Source
-- Semigroup e => Applicative (Validation e)	 
-- (Ord a, Ord e) => Ord (Validation e a)

---- To be done
-- Traversable (Validation e)
-- Bitraversable Validation	 
-- https://github.com/bitemyapp/hedgehog-checkers/issues/9

-- (Eq a, Eq e) => Eq (Validation e a)	 
-- (Show a, Show e) => Show (Validation e a)	 
