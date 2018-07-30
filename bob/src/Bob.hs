module Bob (responseFor) where

responseFor :: String -> String
responseFor "" = "Fine. Be that way!"
responseFor xs =
  case last xs of
    '!' -> "Whoa, chill out!"
    '?' -> "Sure."
    _   -> "Whatever."
