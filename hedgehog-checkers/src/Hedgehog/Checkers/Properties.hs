
{-# LANGUAGE ScopedTypeVariables #-}

module Hedgehog.Checkers.Properties
  (
  -- | Laws
    identity
  , leftIdentity
  , rightIdentity
  , associativity
  , commutativity
  , reflexive
  , transitive
  , symmetric
  , antiSymmetric
  ) where

import Hedgehog

leftIdentity :: (Eq a, Show a)
             => (a -> a -> a)
             -> a
             -> Gen a
             -> PropertyT IO ()
leftIdentity f i gen = do
  x <- forAll gen
  f i x === x

rightIdentity :: (Eq a, Show a)
              => (a -> a -> a)
              -> a
              -> Gen a
              -> PropertyT IO ()
rightIdentity f i gen = do
  x <- forAll gen
  f x i === x

identity :: (Eq a, Show a)
         => (a -> a -> a)
         -> a
         -> Gen a
         -> PropertyT IO ()
identity f i gen = do
  leftIdentity f i gen
  rightIdentity f i gen

associativity :: (Eq a, Show a)
              => (a -> a -> a)
              -> Gen a
              -> PropertyT IO ()
associativity f gen = do
  x <- forAll gen
  y <- forAll gen
  z <- forAll gen
  f x (f y z) === f (f x y) z

commutativity :: (Eq b, Show a, Show b)
              => (a -> a -> b)
              -> Gen a
              -> PropertyT IO ()
commutativity f gena = do
  a <- forAll gena
  a' <- forAll gena
  f a a' === f a' a

reflexive :: (Show a)
          => (a -> a -> Bool)
          -> Gen a
          -> PropertyT IO ()
reflexive rel gena = do
  a <- forAll gena
  assert $ rel a a

transitive :: (Show a)
           => (a -> a -> Bool)
           -> Gen a
           -> (a -> Gen a)
           -> PropertyT IO ()
transitive rel gena genf = do
  a <- forAll gena
  b <- forAll (genf a)
  c <- forAll (genf b)
  ((rel a b) && (rel b c)) === (rel a c)

symmetric :: (Show a)
          => (a -> a -> Bool)
          -> Gen a
          -> (a -> Gen a)
          -> PropertyT IO ()
symmetric rel gena genf = do
  a <- forAll gena
  b <- forAll (genf a)
  (rel a b) === (rel b a)

antiSymmetric :: (Eq a, Show a)
              => (a -> a -> Bool)
              -> Gen a
              -> (a -> Gen a)
              -> PropertyT IO ()
antiSymmetric rel gena genf = do
  a <- forAll gena
  b <- forAll (genf a)
  ((rel a b) && (rel b a)) === (a == b)
