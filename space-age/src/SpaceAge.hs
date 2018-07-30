module SpaceAge (Planet(..), ageOn) where

data Planet =
    Earth
  | Mercury
  | Venus
  | Mars
  | Jupiter
  | Saturn
  | Uranus
  | Neptune
  deriving (Show, Eq)

planetOrbitalPeriod :: Planet -> Float
planetOrbitalPeriod Earth   = 1
planetOrbitalPeriod Mercury = 0.2408467
planetOrbitalPeriod Venus   = 0.61519726
planetOrbitalPeriod Mars    = 1.8808158
planetOrbitalPeriod Jupiter = 11.862615
planetOrbitalPeriod Saturn  = 29.447498
planetOrbitalPeriod Uranus  = 84.016846
planetOrbitalPeriod Neptune = 164.79132

earthYearInSeconds :: Float
earthYearInSeconds = 31557600.0

ageOn :: Planet -> Float -> Float
ageOn planet seconds = seconds / (earthYearInSeconds * planetOrbitalPeriod planet)
