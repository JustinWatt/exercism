module Isogram (isIsogram) where
import Data.List (nub)
import Data.Char (toLower, isAlpha)
isIsogram :: String -> Bool
isIsogram s = nub s' == s'
  where
    s' = filter isAlpha $ fmap toLower s
