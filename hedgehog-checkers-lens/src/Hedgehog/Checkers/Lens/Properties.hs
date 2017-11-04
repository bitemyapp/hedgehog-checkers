{-# LANGUAGE RankNTypes #-}

module Hedgehog.Checkers.Lens.Properties
  ( isSetter
  , isLens
  ) where

import           Control.Lens

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

--------------------------------------------------------------------------------
-- | A 'Setter' is only legal if the following 3 laws hold:
--
-- 1. @set l y (set l x a) ≡ set l y a@
--
-- 2. @over l id ≡ id@
--
-- 3. @over l f . over l g ≡ over l (f . g)@
isSetter :: (Show s, Show a, Eq s)
         => Setter' s a -> Gen a -> Gen s -> PropertyT IO ()
isSetter setter genv gens = do
  settee <- forAll gens
  val <- forAll genv
  val' <- forAll genv
  assert $ setter_id setter settee
  assert $ setter_set_set setter val val' settee
  -- setter_id l .&. setter_composition l .&. setter_set_set l

-- The first setter law:
setter_id :: Eq s => Setter' s a -> s -> Bool
setter_id l s = over l id s == s

--  The second setter law:
-- setter_composition :: Eq s => Setter' s a -> s -> Fun a a -> Fun a a -> Bool
-- setter_composition l s (Fun _ f) (Fun _ g) = over l f (over l g s) == over l (f . g) s

setter_set_set :: ( Eq s
                  , Show a
                  , Show s
                  )
               => Setter' s a
               -> a
               -> a
               -> s
               -> Bool
setter_set_set setter val val' s =
  (set setter val' (set setter val s)) == set setter val' s

-- type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t

-- 1) You get back what you put in:
-- view l (set l v s)  ≡ v

lens_set_view :: ( Eq a
                 , Show a
                 , Show s
                 )
              => Lens' s a
              -> a
              -> s
              -> Bool
lens_set_view setter val s = do
  (view setter (set setter val s)) == val

-- 2) Putting back what you got doesn't change anything:
-- set l (view l s) s  ≡ s

lens_view_set :: ( Eq s
                 , Show s
                 )
              => Lens' s a
              -> s
              -> Bool
lens_view_set setter s = do
  (set setter (view setter s) s) == s

-- 3) Setting twice is the same as setting once:
-- set l v' (set l v s) ≡ set l v' s

-- isLens l = lens_set_view l .&. lens_view_set l .&. isTraversal l

isLens :: ( Eq a
          , Eq s
          , Show a
          , Show s
          )
       => Lens' s a
       -> Gen a
       -> Gen s
       -> PropertyT IO ()
isLens lens genv gens = do
  settee <- forAll gens
  val <- forAll genv
  assert $ lens_set_view lens val settee
  assert $ lens_view_set lens settee
  isSetter lens genv gens

-- Group "Lens laws" [ ("view l (set l v s)  ≡ v"
--                     , viewOfSet)
--                   , ("set l (view l s) s  ≡ s"
--                     , setOfView)
--                   , ("set l v' (set l v s) ≡ set l v' s"
--                     , doubleSet)
--                   ]

-- type Prism s t a b = forall p f. (Choice p, Applicative f) => p a (f b) -> p s (f t)

-- First, if I re or review a value with a Prism and then preview or use (^?), I will get it back:
-- preview l (review l b) ≡ Just b
previewOfReviewIdentity ::
                         ( Eq b
                         , Show b
                         )
                        => Prism' s b
                        -> Gen b
                        -> PropertyT IO ()
previewOfReviewIdentity prism genb = do
  b <- forAll genb
  (preview prism (review prism b)) === (Just b)

-- Second, if you can extract a value a using a Prism l from a value s, then the value s is completely described by l and a:
-- If preview l s ≡ Just a then review l a ≡ s
previewJustReviewIdentity :: PropertyT IO ()
previewJustReviewIdentity = undefined
