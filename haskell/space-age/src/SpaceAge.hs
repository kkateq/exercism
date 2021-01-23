module SpaceAge (Planet (..), ageOn) where

data Planet
  = Mercury
  | Venus
  | Earth
  | Mars
  | Jupiter
  | Saturn
  | Uranus
  | Neptune

earthOrbitalPeriodInSeconds :: Float
earthOrbitalPeriodInSeconds = 31557600

planetOrbitalPeriodInYears :: Planet -> Float
planetOrbitalPeriodInYears planet = case planet of
  Mercury -> 0.2408467
  Earth -> 1
  Mars -> 1.8808158
  Jupiter -> 11.862615
  Saturn -> 29.447498
  Uranus -> 84.016846
  Neptune -> 164.79132
  Venus -> 0.61519726

ageOn :: Planet -> Float -> Float
ageOn planet seconds =
  seconds / earthOrbitalPeriodInSeconds
    / planetOrbitalPeriodInYears planet
