module Acronym (abbreviate) where

import Data.Char (toUpper, isUpper)
import Data.Function ((&))
import Data.List.Split

splitWhenKeepDelims :: (a -> Bool) -> [a] -> [[a]]
splitWhenKeepDelims = split . keepDelimsR . whenElt

capitalize :: String -> String
capitalize "" = ""
capitalize (x:xs) = toUpper x : xs

abbreviate :: String -> String
abbreviate = foldMap (filter isUpper . capitalize) . words
