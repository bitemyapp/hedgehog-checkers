{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative
import           Control.Monad
import           Data.Either.Validation
import           Data.Functor (void)
import           Data.Monoid (Sum(..))
import           System.Exit (exitFailure)

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Hedgehog.Checkers

genValidation :: Gen a -> Gen b -> Gen (Validation a b)
genValidation ga gb = do
  a <- ga
  b <- gb
  Gen.choice [return $ Failure a, return $ Success b]

validationAlternative :: Property
validationAlternative = property $ do
  let genSumInt = Sum <$> Gen.int (Range.linear 0 maxBound)
      genVal = genValidation genSumInt genSumInt
  alternative genVal

genInt :: Gen Int
genInt = Gen.int (Range.linear 0 maxBound)

genSum :: Gen (Sum Int)
genSum = Sum <$> genInt

genEither' :: Gen a -> Gen b -> Gen (Either a b)
genEither' ga gb = do
  a <- ga
  b <- gb
  Gen.choice [return $ Left a, return $ Right b]

genEither :: Gen (Either Int Int)
genEither = genEither' genInt genInt

eitherAlt :: Property
eitherAlt = property $ do
  alt genEither

eitherBifunctor :: Property
eitherBifunctor = property $ do
  bifunctor genEither genInt genInt genInt

eitherFunctor :: Property
eitherFunctor = property $ do
  functor genEither

eitherApply :: Property
eitherApply = property $ do
  apply genEither genInt genInt genInt

eitherApplicative :: Property
eitherApplicative = property $ do
  applicative genEither genInt genInt genInt

eitherSemigroup :: Property
eitherSemigroup = property $ do
  semigroup genEither

genMaybe' :: Gen a -> Gen (Maybe a)
genMaybe' ga =
  -- I need to bias this to Just
  Gen.choice [return Nothing, Just <$> ga]

genMaybe :: Gen (Maybe (Sum Int))
genMaybe = genMaybe' genSum

maybeMonoid :: Property
maybeMonoid = property $ do
  monoid genMaybe

maybeAlt :: Property
maybeAlt = property $ alt genMaybe

maybeAlternative :: Property
maybeAlternative = property $ alternative genMaybe

maybeAlternativeAlt :: Property
maybeAlternativeAlt = property $ alternativeAltAgreement genMaybe

maybeApply :: Property
maybeApply = property $
  apply genMaybe genSum genSum genSum

maybeApplicative :: Property
maybeApplicative = property $
  applicative genMaybe genSum genSum genSum

maybeApplicativeApply :: Property
maybeApplicativeApply = property $
  applicativeApplyAgreement genMaybe genSum genSum

intOrd :: Property
intOrd = property $
  ord genInt varyGenInt
  where varyGenInt i =
          Gen.int (Range.linear i maxBound)

main :: IO ()
main = do
  e <-
    checkParallel $
      Group "Data.Either" [ ("Alt", eitherAlt)
                          , ("Bifunctor", eitherBifunctor)
                          , ("Functor", eitherFunctor)
                          , ("Semigroup", eitherSemigroup)
                          , ("Apply", eitherApply)
                          , ("Applicative", eitherApplicative)
                          ]
  m <-
    checkParallel $
      Group "Data.Maybe" [ ("Monoid", maybeMonoid)
                         , ("Alt", maybeAlt)
                         , ("Alternative", maybeAlternative)
                         , ("AlternativeAlt", maybeAlternativeAlt)
                         , ("Apply", maybeApply)
                         , ("Applicative", maybeApplicative)
                         , ("ApplicativeApply", maybeApplicativeApply)
                         ]
  o <-
    checkParallel $
      Group "Ord" [ ("Int", intOrd)
                  ]
  unless (and [e,m,o]) exitFailure
