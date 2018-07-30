module DifferenceOfSquares exposing (..)

import List exposing (sum, map)

square : number -> number
square n = n * n

squareOfSum : number -> number
squareOfSum n =
  sum [1..n]
  |> square
  
sumOfSquares n =
  map square [1..n]
  |> sum

difference n =
  squareOfSum n - sumOfSquares n 

