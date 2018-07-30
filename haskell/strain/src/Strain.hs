module Strain (keep, discard) where

discard :: (a -> Bool) -> [a] -> [a]
discard p xs = foldr discard' [] xs
  where
    discard' x acc =
      if p x
      then acc
      else x : acc

keep :: (a -> Bool) -> [a] -> [a]
keep p xs = foldr keep' [] xs
  where
    keep' x acc =
      if p x
      then x : acc
      else acc
