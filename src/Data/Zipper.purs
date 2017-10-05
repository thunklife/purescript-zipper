module Data.Zipper 
  ( Zipper
  , fromList
  , singleton
  , current
  , forward
  , backward
  , toList
  , length
  ) where

import Data.Foldable (class Foldable, foldl, foldr, foldMap)
import Data.List ((:), reverse, List(..))
import Data.List as L
import Data.Maybe (Maybe(..))
import Data.Monoid (class Monoid)
import Prelude (class Apply, class Functor, class Semigroup, class Show, show, (+), (<$>), (<*>), (<>), ($))

data Zipper a = 
    Empty
  | Zipper (List a) a (List a)

instance showZipper :: Show a => Show (Zipper a) where
  show Empty          = "Empty"
  show (Zipper b c a) = "Zipper " <> (show b) <> " " <> (show c) <> " " <> (show a)

instance semigroupZipper :: Semigroup a => Semigroup (Zipper a) where
  append zs Empty                         = zs
  append Empty zs                         = zs
  append (Zipper b c a) (Zipper b' c' a') = (Zipper (b <> b') (c <> c') (a <> a'))

instance monoidZipper :: Semigroup a => Monoid (Zipper a) where
  mempty = Empty

instance mapZipper :: Functor Zipper where
  map _ Empty          = Empty
  map f (Zipper b c a) = Zipper (f <$> b) (f c) (f <$> a)

instance foldableZipper :: Foldable Zipper where
  foldl f b zs = foldl f b $ toList zs
  foldr f b zs = foldr f b $ toList zs
  foldMap f zs = foldMap f $ toList zs

instance applyZipper :: Apply Zipper where
  apply (Zipper bf cf af) (Zipper b c a) = (Zipper (bf<*>b) (cf c) (af <*> a))
  apply _ _ = Empty

fromList :: forall a. List a -> Zipper a
fromList Nil = Empty
fromList (Cons x xs) = Zipper Nil x xs

singleton :: forall a. a -> Zipper a
singleton x = Zipper Nil x Nil

current :: forall a. Zipper a -> Maybe a
current Empty = Nothing
current (Zipper _ c _) = Just c

forward :: forall a. Zipper a -> Zipper a
forward Empty = Empty
forward zipper@(Zipper _ _ Nil) = zipper
forward (Zipper b c (Cons a as)) = Zipper (c : b) a as

backward :: forall a. Zipper a -> Zipper a
backward Empty = Empty
backward zipper@(Zipper Nil _ _) = zipper
backward (Zipper (Cons b bs) c a) = Zipper bs b (c : a)

toList :: forall a. Zipper a -> List a
toList Empty = Nil
toList (Zipper b c a) = (reverse b) <> (c : a)

length :: forall a. Zipper a -> Int
length Empty = 0
length (Zipper b _ c) = 1 + (L.length b) + (L.length c)