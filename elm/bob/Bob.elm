module Bob exposing (..)

import Maybe exposing (..)
import String exposing (..)


last : List a -> Maybe a
last xs = List.reverse xs |> List.head

yelling : List Char -> Bool


hey : String -> String
hey sentence =
  let
      charList = String.toList sentence
      finalChar = String.toList sentence |> last
      secondChar = 
  case String.toList sentence |> last of
    Just '?' -> 
      "Sure."
    Just '!' ->
      "Whoa, chill out!"
    Nothing ->
      "Fine. Be that way!"
    _ ->
      "Whatever."
:
