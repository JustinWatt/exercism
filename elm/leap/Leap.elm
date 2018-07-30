module Leap exposing (..)

divisible : Int -> Int -> Bool
divisible n d =
  n % d == 0

isLeapYear : Int -> Bool
isLeapYear year =
  divisible year 4 && not (divisible year 100) || divisible year 400
