module WordCount (wordCount) where

import Data.List (foldl', group, sort)
import Data.Char (isAlphaNum, toLower)
import Data.Function ((&))

replacePunctuation :: (Char -> Bool) -> Char -> Char
replacePunctuation p char
  | p char    = char
  | otherwise = ' '

isQuote :: Char -> Bool
isQuote = (==) '\''

isContraction :: String -> Bool
isContraction = ((==) 1) . length . filter isQuote

stripStrayPunctuation :: String -> String
stripStrayPunctuation =
  fmap (replacePunctuation (\x -> isAlphaNum x || isQuote x))

cleanContractions :: String -> String
cleanContractions s =
  if isContraction s then s else filter (not . isQuote) s

wordCount :: String -> [(String, Int)]
wordCount xs =
    xs
  & fmap toLower
  & stripStrayPunctuation
  & words
  & fmap cleanContractions
  & sort
  & group
  & fmap (\ws -> (head ws, length ws))
