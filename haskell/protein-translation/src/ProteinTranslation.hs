module ProteinTranslation(proteins) where

import Data.Map (Map)
import qualified Data.Map as M
import Data.List.Split (chunksOf)
import Data.Maybe (catMaybes, isJust)
import Data.Function ((&))

proteinMap :: Map String String
proteinMap =
  M.fromList [("AUG", "Methionine")
             ,("UUU", "Phenylalanine")
             ,("UUC", "Phenylalanine")
             ,("UUA", "Leucine")
             ,("UUG", "Leucine")
             ,("UCU", "Serine")
             ,("UCC", "Serine")
             ,("UCA", "Serine")
             ,("UCG", "Serine")
             ,("UAU", "Tyrosine")
             ,("UAC", "Tyrosine")
             ,("UGU", "Cysteine")
             ,("UGC", "Cysteine")
             ,("UGG", "Tryptophan")
             ]

proteins :: String -> Maybe [String]
proteins s =
    chunksOf 3 s
  & fmap (flip M.lookup proteinMap)
  & takeWhile isJust
  & catMaybes
  & Just
