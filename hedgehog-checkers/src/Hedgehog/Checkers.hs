module Hedgehog.Checkers
  (
  -- * Classes
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

  -- * Laws
  , identity
  , leftIdentity
  , rightIdentity
  , associativity
  , commutativity
  , reflexive
  , transitive
  , symmetric
  , antiSymmetric
  ) where

import Hedgehog.Checkers.Classes
import Hedgehog.Checkers.Properties
