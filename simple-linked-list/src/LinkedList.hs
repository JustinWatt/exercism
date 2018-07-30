module LinkedList
    ( LinkedList
    , datum
    , fromList
    , isNil
    , new
    , next
    , nil
    , reverseLinkedList
    , toList
    ) where
import Data.List
data LinkedList a =
    Nil
  | Cons a (LinkedList a)

  deriving (Eq, Show)

datum :: LinkedList a -> a
datum Nil = error "empty head"
datum (Cons a l) = a

fromList :: [a] -> LinkedList a
fromList [] = Nil
fromList (x:xs) = Cons x $ fromList xs

isNil :: LinkedList a -> Bool
isNil Nil = True
isNil _ = False

new :: a -> LinkedList a -> LinkedList a
new = Cons

next :: LinkedList a -> LinkedList a
next (Cons a l@(Cons _ _)) = l
next _ = Nil

nil :: LinkedList a
nil = Nil

instance Foldable LinkedList where
  foldr _ z Nil = z
  foldr f z (Cons a ll) = foldr f (f a z) ll

reverseLinkedList :: LinkedList a -> LinkedList a
reverseLinkedList = foldl' (flip new) Nil

toList :: LinkedList a -> [a]
toList Nil = []
toList (Cons a linkedList) = a : toList linkedList
