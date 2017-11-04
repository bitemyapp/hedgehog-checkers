{-# LANGUAGE ScopedTypeVariables #-}

module Hedgehog.Checkers.Classes
  (
  -- | Classes
    alt
  , alternative
  , bifunctor
  , functor
  , semigroup
  , monoid
  , applicative
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
          fbc' <- ordFuncWtf genb genc
          let fbc = fmap (const fbc') fa
          fab' <- ordFuncWtf gena genb
          let fab = fmap (const fab') fa
          (pure (.) <*> fbc <*> fab <*> fa) === (fbc <*> (fab <*> fa))

        applicativeHomomorphism = do
          a <- forAll gena
          f <- ordFuncWtf gena genb
          let p :: x -> f x
              p = pure
          (p f <*> p a) === p (f a)

        applicativeInterchange = do
          a <- forAll gena
          fa <- forAll gen
          fab' <- ordFuncWtf gena genb
          let fab = fmap (const fab') fa
          (fab <*> pure a) === (pure ($ a) <*> fab)

        applicativeFunctor = do
          fa <- forAll gen
          f <- ordFuncWtf gena genb
          fmap f fa === (pure f <*> fa)

-- identityP     :: m a -> Property
-- compositionP  :: m (b -> c) -> m (a -> b) -> m a -> Property
-- homomorphismP :: (a -> b) -> a -> Property
-- interchangeP  :: m (a -> b) -> a -> Property
-- functorP      :: (a -> b) -> m a -> Property

-- identityP v        = (pure id <*> v) =-= v
-- compositionP u v w = (pure (.) <*> u <*> v <*> w) =-= (u <*> (v <*> w))
-- homomorphismP f x  = (pure f <*> pure x) =-= (pure (f x) :: m b)
-- interchangeP u y   = (u <*> pure y) =-= (pure ($ y) <*> u)
-- functorP f x       = (fmap f x) =-= (pure f <*> x)

-- (Semigroup e, Monoid e) => Alternative (Validation e)	 
-- Alt (Validation e)	 
-- Functor (Validation e)
-- Bifunctor Validation
-- Semigroup e => Semigroup (Validation e a)	 
-- Monoid e => Monoid (Validation e a)Source

-- Semigroup e => Applicative (Validation e)	 

-- Traversable (Validation e)
-- Bitraversable Validation	 

-- (Eq a, Eq e) => Eq (Validation e a)	 
-- (Ord a, Ord e) => Ord (Validation e a)	 
-- (Show a, Show e) => Show (Validation e a)	 
