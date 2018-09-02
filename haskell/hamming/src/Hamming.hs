module Hamming (distance) where


countDifferences :: (Eq a) => [a] -> [a] -> Int
countDifferences = sum . zipWith (\x y -> if x /= y then 1 else 0)

distance :: String -> String -> Maybe Int
distance xs ys =
  if length xs /= length ys
  then Nothing
  else Just $ countDifferences xs ys
