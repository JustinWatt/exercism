module Strain exposing (..)

keep : (a -> Bool) -> List a -> List a
keep p xs =
  case xs of
    [] -> []
    x :: xs ->
      if p x 
      then x :: keep p xs
      else keep p xs

discard : (a -> Bool) -> List a -> List a
discard p xs = keep (not << p) xs
