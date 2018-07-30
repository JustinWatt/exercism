module Pangram exposing (..)

import String
import List

alphabet : List Char
alphabet = 
  ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J'
  ,'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T'
  ,'U', 'V', 'W', 'X', 'Y', 'Z']

isPangram : String -> Bool
isPangram s =
  let 
    formattedStringList = (String.toList << String.toUpper) s

  in
    List.all (\x -> x `List.member` formattedStringList) alphabet

   

