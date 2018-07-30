module Poker (bestHands) where

import Data.Char (isDigit)
import Data.List (partition)
import Data.List.Split (splitOn)

data Suit =
    Clubs
  | Diamonds
  | Hearts
  | Spades
  deriving (Show, Eq)

data Rank =
    One
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Jack
  | Queen
  | King
  | Ace
  deriving (Show, Eq, Ord)

data Card =
  Card Rank Suit

parseSuit :: String -> Maybe Suit
parseSuit "C" = Just Clubs
parseSuit "H" = Just Hearts
parseSuit "S" = Just Spades
parseSuit "D" = Just Diamonds
parseSuit _   = Nothing

parseRank :: String -> Maybe Rank
parseRank "1"  = Just One
parseRank "2"  = Just Two
parseRank "3"  = Just Three
parseRank "4"  = Just Four
parseRank "5"  = Just Five
parseRank "6"  = Just Six
parseRank "7"  = Just Seven
parseRank "8"  = Just Eight
parseRank "9"  = Just Nine
parseRank "10" = Just Nine
parseRank "J"  = Just Jack
parseRank "Q"  = Just Queen
parseRank "K"  = Just King
parseRank "A"  = Just Ace
parseRank _    = Nothing

parseCard :: String -> Maybe Card
parseCard s =
  Card <$> parseRank rank
       <*> parseSuit suit
  where
   (rank, suit) = partition isDigit s

parseHand :: String -> Maybe [Card]
parseHand hand =
  traverse parseCard $ splitOn " " hand

bestHands :: [String] -> Maybe [String]
bestHands h = do
  hands <- traverse parseHand h

  undefined
