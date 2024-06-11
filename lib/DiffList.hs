module DiffList where

newtype DiffList a = DiffList {getDiffList :: [a] -> [a]}

instance Semigroup (DiffList a) where
  (<>) (DiffList l1) (DiffList l2) = DiffList (l1 . l2)

instance Monoid (DiffList a) where
  mempty = DiffList id

appendItem :: a -> DiffList a -> DiffList a
appendItem x (DiffList f) = DiffList (f . (x :))

toList :: DiffList a -> [a]
toList (DiffList f) = f []

fromList :: [a] -> DiffList a
fromList xs = DiffList (xs ++)

