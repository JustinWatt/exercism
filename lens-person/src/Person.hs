{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Person
  ( Address (..)
  , Born    (..)
  , Name    (..)
  , Person  (..)
  , bornStreet
  , renameStreets
  , setBirthMonth
  , setCurrentStreet
  ) where

import           Data.Time.Calendar (Day, fromGregorian, toGregorian)
import           Lens.Micro
import           Lens.Micro.TH

data Person =
  Person { _name    :: Name
         , _born    :: Born
         , _address :: Address
         }

data Name =
  Name { _foreNames :: String
       , _surName   :: String
       }

data Born =
  Born { _bornAt :: Address
       , _bornOn :: Day
       }

data Address =
  Address { _street      :: String
          , _houseNumber :: Int
          , _place       :: String
          , _country     :: String
          }

makeLenses ''Person
makeLenses ''Name
makeLenses ''Born
makeLenses ''Address
makeLenses ''Day

bornStreet :: Born -> String
bornStreet born = born ^. bornAt . street

setCurrentStreet :: String -> Person -> Person
setCurrentStreet s person =
  person
  & address . street .~ s

setBirthMonth :: Int -> Person -> Person
setBirthMonth month person =
  person
  & born . bornOn %~ (f month)
  where
    f :: Int -> Day -> Day
    f month day = fromGregorian y m d
      where
        (y, m, d) = toGregorian day & _2 .~ month

renameStreets :: (String -> String) -> Person -> Person
renameStreets f person =
  person
  & born . bornAt .  street %~ f
  & address . street %~ f
