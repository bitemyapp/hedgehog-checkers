{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes          #-}

module Hedgehog.Checkers.Lens.Properties
  ( isSetter
  , isLens
  , isIso
  , isPrism
  , isTraversal
  ) where

import           Control.Applicative
import           Data.Functor.Compose

import           Control.Lens

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Hedgehog.Checkers.Ugly.Function.Hack

-----------------------------------------------------------
-- | A 'Setter' is only legal if the following 3 laws hold:
--
-- 1. @set l y (set l x a) ≡ set l y a@
--
-- 2. @over l id ≡ id@
--
-- 3. @over l f . over l g ≡ over l (f . g)@
isSetter :: (Show s, Show a, Eq s)
         => Setter' s a
         -> Gen a
         -> Gen s
         -> Gen (a -> a)
         -> PropertyT IO ()
isSetter setter genv gens genf = do
  settee <- forAll gens
  val <- forAll genv
  val' <- forAll genv
  f <- funcForAllWtf genf
  g <- funcForAllWtf genf
  assert $ setter_id setter settee
  assert $ setter_composition setter settee f g
  assert $ setter_set_set setter val val' settee

-- The first setter law:
setter_id :: Eq s => Setter' s a -> s -> Bool
setter_id l s = over l id s == s

--  The second setter law:
setter_composition :: Eq s
                   => Setter' s a
                   -> s
                   -> (a -> a)
                   -> (a -> a)
                   -> Bool
setter_composition l s f g =
  over l f (over l g s) == over l (f . g) s

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

isLens :: ( Eq a
          , Eq s
          , Show a
          , Show s
          )
       => Lens' s a
       -> Gen a
       -> Gen s
       -> Gen (a -> a)
       -> PropertyT IO ()
isLens lens genv gens genf = do
  settee <- forAll gens
  val <- forAll genv
  assert $ lens_set_view lens val settee
  assert $ lens_view_set lens settee
  isSetter lens genv gens genf

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


isIso :: ( Eq a
         , Eq s
         , Show a
         , Show s
         )
      => Iso' s a
      -> Gen a
      -> Gen s
      -> Gen (a -> a)
      -> Gen (s -> s)
      -> PropertyT IO ()
isIso l gena gens genf genfs = do
  a <- forAll gena
  s <- forAll gens
  assert $ iso_hither l s
  assert $ iso_yon l a
  isLens l gena gens genf
  isLens (from l) gens gena genfs

-- isIso :: (Arbitrary s, Arbitrary a, CoArbitrary s, CoArbitrary a, Show s, Show a, Eq s, Eq a, Function s, Function a)
--       => Iso' s a -> Property
-- isIso l = iso_hither l .&. iso_yon l .&. isLens l .&. isLens (from l)

iso_hither :: Eq s => AnIso' s a -> s -> Bool
iso_hither l s = s ^. cloneIso l . from l == s

iso_yon :: Eq a => AnIso' s a -> a -> Bool
iso_yon l a = a ^. from l . cloneIso l == a

-- 3) Setting twice is the same as setting once:
-- set l v' (set l v s) ≡ set l v' s

-- type Prism s t a b = forall p f. (Choice p, Applicative f) => p a (f b) -> p s (f t)

isPrism :: ( Show s
           , Show a
           , Eq s
           , Eq a
           )
        => Prism' s a
        -> Gen a
        -> Gen s
        -> Gen (a -> a)
        -> PropertyT IO ()
isPrism l gena gens genf = do
  a <- forAll gena
  s <- forAll gens
  assert $ prism_yin l a
  assert $ prism_yang l s
  isTraversal l gena gens genf

-- isPrism :: (Arbitrary s, Arbitrary a, CoArbitrary a, Show s, Show a, Eq s, Eq a, Function a)
--       => Prism' s a -> Property
-- isPrism l = isTraversal l .&. prism_yin l .&. prism_yang l

prism_yin :: Eq a => Prism' s a -> a -> Bool
prism_yin l a = preview l (review l a) == Just a

prism_yang :: Eq s => Prism' s a -> s -> Bool
prism_yang l s = maybe s (review l) (preview l s) == s

-- First, if I re or review a value with a Prism and then preview or use (^?), I will get it back:
-- preview l (review l b) ≡ Just b
-- previewOfReviewIdentity ::
--                          ( Eq b
--                          , Show b
--                          )
--                         => Prism' s b
--                         -> Gen b
--                         -> PropertyT IO ()
-- previewOfReviewIdentity prism genb = do
--   b <- forAll genb
--   (preview prism (review prism b)) === (Just b)

-- Second, if you can extract a value a using a Prism l from a value s, then the value s is completely described by l and a:
-- If preview l s ≡ Just a then review l a ≡ s
-- previewJustReviewIdentity :: PropertyT IO ()
-- previewJustReviewIdentity = undefined

-- | A 'Traversal' is only legal if it is a valid 'Setter' (see 'isSetter' for
-- what makes a 'Setter' valid), and the following laws hold:
--
-- 1. @t pure ≡ pure@
--
-- 2. @fmap (t f) . t g ≡ getCompose . t (Compose . fmap f . g)@
isTraversal :: ( Eq s
               , Show a
               , Show s
               )
            => Traversal' s a
            -> Gen a
            -> Gen s
            -> Gen (a -> a)
            -> PropertyT IO ()
isTraversal l gena gens genf = do
  s <- forAll gens
  as <- forAll (Gen.list (Range.linear 0 50) gena)
  bs <- forAll (Gen.list (Range.linear 0 50) gena)
  t <- forAll Gen.bool
  assert $ traverse_pureMaybe l s
  assert $ traverse_pureList l s
  assert $ traverse_compose
            l
            (\x -> as ++ [x] ++ bs)
            (\x -> if t then Just x else Nothing)
            s
  isSetter l gena gens genf

traverse_pure :: forall f s a
               . ( Applicative f
                 , Eq (f s)
                 )
              => LensLike' f s a
              -> s
              -> Bool
traverse_pure l s = l pure s == (pure s :: f s)

traverse_pureMaybe :: Eq s
                   => LensLike' Maybe s a
                   -> s
                   -> Bool
traverse_pureMaybe = traverse_pure

traverse_pureList :: Eq s
                  => LensLike' [] s a
                  -> s
                  -> Bool
traverse_pureList = traverse_pure

traverse_compose :: ( Applicative f
                    , Applicative g
                    , Eq (f (g s))
                    )
                 => Traversal' s a
                 -> (a -> g a)
                 -> (a -> f a)
                 -> s
                 -> Bool
traverse_compose t f g s =
  (fmap (t f) . t g) s == (getCompose . t (Compose . fmap f . g)) s
