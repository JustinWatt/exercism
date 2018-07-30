module IsbnVerifier (isbn) where

import           Data.Char (isDigit)
import           Data.List.Split (chunksOf)
import           Debug.Trace


isZero :: Int -> Bool
isZero x = x == 0

(|>) :: a -> (a -> c) -> c
(|>) = flip ($)

isbn :: String -> Bool
isbn s = and [ validateParseDigit (last cleanedInput)
             , all isDigit isbnDigitChars
             , correctLength cleanedInput
             , checkValidity isbnDigits
             ]

  where
    cleanedInput = cleanInput s

    isbnDigits :: [Int]
    isbnDigits = readISBNDigits isbnDigitChars ++ [parseCheckDigit [checkDigitChar]]

    isbnDigitChars :: String
    isbnDigitChars = take 9 cleanedInput

    checkDigitChar :: Char
    checkDigitChar = last cleanedInput

    cleanInput :: String -> String
    cleanInput = filter ('-' /=)

    correctLength :: [a] -> Bool
    correctLength = (10 ==) . length

    validateParseDigit :: Char -> Bool
    validateParseDigit 'X' = True
    validateParseDigit x   = isDigit x

    parseCheckDigit :: String -> Int
    parseCheckDigit "X" = 10
    parseCheckDigit  x  = read x

    readISBNDigits :: String -> [Int]
    readISBNDigits = fmap read . chunksOf 1

    checkValidity :: [Int] -> Bool
    checkValidity digits =
      zipWith (*) (reverse [1..10]) digits
      |> sum
      |> flip mod 11
      |> isZero

