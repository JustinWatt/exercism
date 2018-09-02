module RunLength (decode, encode) where

import Data.List (group, partition)
import Data.Char (isAlpha, isDigit)
import Text.Regex

decode :: String -> String
decode = foldMap decodeGroup . splitRegex (mkRegex "\\d*\\w")
  where
    decodeGroup :: String -> String
    decodeGroup "" = ""
    decodeGroup [x] = [x]
    decodeGroup g =
      let (valStr, (x:_)) = partition isDigit g
      in
        replicate (read valStr) x


encode :: String -> String
encode = foldMap encodeGroup . group
  where
    encodeGroup :: String -> String
    encodeGroup "" = ""
    encodeGroup [x] = [x]
    encodeGroup g@(x:_) = (show $ length g) ++ [x]
