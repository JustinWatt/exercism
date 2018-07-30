module Deque (Deque, mkDeque, pop, push, shift, unshift) where

data Deque a =
    Nil
  | Cons (Deque a) a (Deque a)
  deriving Show

mkDeque :: IO (Deque a)
mkDeque = do
  return Nil

pop :: Deque a -> IO (Maybe a)
pop Nil = return Nothing
pop (Cons _ x Nil) = return $ Just x
pop (Cons _ _ r) = pop r

push :: Deque a -> a -> IO ()
push Nil x = return $ Cons Nil x Nil
push (Cons _ _ r@(Cons _ _ _)) x = push r x
push r@(Cons l a Nil) x = do
  return $ (Cons l a (Cons r x Nil))

unshift :: Deque a -> a -> IO ()
unshift deque x = do
  return $ Cons Nil x deque

shift :: Deque a -> IO (Maybe a)
shift Nil = return Nothing
shift (Cons Nil x deque) = return $ Just x
