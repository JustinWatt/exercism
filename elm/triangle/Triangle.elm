module Triangle exposing (..)

import List
import Set

invalidSides : List number -> Bool
invalidSides sides =
  List.any ((>=) 0) sides

uniqueSides : List comparable -> Int
uniqueSides = Set.size << Set.fromList

triangleKind : number -> number -> number -> Result String String
triangleKind s1 s2 s3 =
  if invalidSides [s1, s2, s3]
  then Err "Invalid lengths"

  else if s1 + s2 <= s3 || s1 + s3 <= s2 || s2 + s3 <= s1 
  then Err "Violates inequality"

  else 
    case uniqueSides [s1, s2, s3] of
      3 -> Ok "scalene"
      2 -> Ok "isosceles"
      _ -> Ok "equilateral"

