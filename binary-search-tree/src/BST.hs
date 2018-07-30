module BST
    ( BST(..)
    , bstLeft
    , bstRight
    , bstValue
    , empty
    , fromList
    , insert
    , singleton
    , toList
    ) where

import Data.Foldable (fold)

data BST a =
    Leaf
  | BST (BST a) a (BST a)
  deriving (Eq, Show)

bstLeft :: BST a -> Maybe (BST a)
bstLeft (BST l@(BST _ _ _) _ _) = Just l
bstLeft _ = Nothing

bstRight :: BST a -> Maybe (BST a)
bstRight (BST _ _ r@(BST _ _ _)) = Just r
bstRight _ = Nothing

bstValue :: BST a -> Maybe a
bstValue (BST _ x _) = Just x
bstValue _ = Nothing

empty :: BST a
empty = Leaf

fromList :: Ord a => [a] -> BST a
fromList xs = foldr insert empty xs

insert :: Ord a => a -> BST a -> BST a
insert x Leaf = singleton x
insert x (BST l y r)
  | x <= y    = BST (insert x l) y r
  | otherwise = BST l y (insert x r)

singleton :: a -> BST a
singleton x = BST Leaf x Leaf

instance Foldable BST where
  foldr f z Leaf = z
  foldr f z (BST l x r) =
    foldr f (foldr f (f x z) l) r

toList :: BST a -> [a]
toList Leaf = []
toList (BST l x r) = fold [toList l, [x], toList r]
