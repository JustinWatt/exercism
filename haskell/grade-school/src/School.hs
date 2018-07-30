module School (School, add, empty, grade, sorted) where

import           Data.List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Monoid     ((<>))

data School = School (Map Int [String])

add :: Int -> String -> School -> School
add gradeNum student (School s) =
  School $ Map.insertWith sortAppend gradeNum [student] s
  where
    sortAppend x xs = sort (x <> xs)

empty :: School
empty = School Map.empty

grade :: Int -> School -> [String]
grade gradeNum (School s) = Map.findWithDefault [] gradeNum s

sorted :: School -> [(Int, [String])]
sorted (School s) = Map.toList s
