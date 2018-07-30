{-# LANGUAGE ScopedTypeVariables #-}
module ETL (transform) where

import Data.Map (Map)
import Data.Char (toLower)
import qualified Data.Map as M

transform :: forall a. Map a String -> Map Char a
transform legacyData =
  M.foldrWithKey extract M.empty legacyData
  where
    extract :: a -> String -> Map Char a -> Map Char a
    extract k s acc = foldr (load k) acc s

    load :: a -> Char -> Map Char a -> Map Char a
    load score letter db = M.insert (toLower letter) score db
