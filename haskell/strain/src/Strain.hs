module Strain (keep, discard) where

keep :: (a -> Bool) -> [a] -> [a]
keep p = foldr keep' []
  where
    keep' x acc =
      if p x
      then x : acc
      else acc

discard :: (a -> Bool) -> [a] -> [a]
discard p = keep (not . p)
