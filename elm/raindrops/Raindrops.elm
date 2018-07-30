module Raindrops exposing (..)

translateDrop : Int -> Int -> String -> String
translateDrop n d w =
  if n % d == 0 then w else ""

raindrops : Int -> String
raindrops n =
  let 
     pling = translateDrop n 3 "Pling"
     plang = translateDrop n 5 "Plang"
     plong = translateDrop n 7 "Plong"
     result = pling ++ plang ++ plong
  in
     if result /= "" then 
       result 
     else 
       (toString n)

