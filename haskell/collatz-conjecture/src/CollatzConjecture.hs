module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz n | n > 0     = Just (go 0 n)
          | otherwise = Nothing
  where
    go :: Integer -> Integer -> Integer
    go steps x | x == 1    = steps
               | even x    = go (steps + 1) (div x 2)
               | otherwise = go (steps + 1) (1 + (x * 3))


