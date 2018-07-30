module SumOfMultiples (sumOfMultiples) where
import Data.List (nub, sort)
import Debug.Trace

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit =
  sum $ nub $ sort [ x |
                     x <- [2 .. limit]
                   , any (multipleOf x) factors
                   ]
  where
    multipleOf x y = mod x y == 0
