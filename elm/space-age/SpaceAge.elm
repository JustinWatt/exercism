module SpaceAge exposing (..)

type Planet
  = Mercury
  | Venus
  | Earth
  | Mars
  | Jupiter
  | Saturn
  | Uranus
  | Neptune

earthYear : Float
earthYear = 31557600.0

planetToEarthYear : Float -> Float -> Float
planetToEarthYear seconds orbitalPeriod =
  seconds / (earthYear * orbitalPeriod)

ageOn : Planet -> Float -> Float
ageOn p seconds =
  let 
    yearOnPlanet = planetToEarthYear seconds
  in
    case p of
      Earth -> 
        yearOnPlanet 1
      Mercury -> 
        yearOnPlanet 0.2408467
      Venus ->
        yearOnPlanet 0.61519726
      Mars ->
        yearOnPlanet 1.8808158
      Jupiter ->
        yearOnPlanet 11.862615
      Saturn ->
        yearOnPlanet 29.447498
      Uranus ->
        yearOnPlanet 84.016846
      Neptune ->
        yearOnPlanet 164.79132
