module Pangram (isPangram) where
import           Data.Char (toLower, isLetter)
import           Data.List (sort, nub)
alphabet :: String
alphabet = ['a'..'z']

(|>) :: a -> (a -> c) -> c
(|>) = flip ($)

isPangram :: String -> Bool
isPangram text =
  map toLower text
  |> filter isLetter
  |> sort
  |> nub
  |> (==) alphabet

