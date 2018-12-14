module SpaceAge (Planet(..), ageOn) where

-- define a typeclass Orbital a,
-- defining toYears a seconds 
-- to convert seconds to years
class Orbital a where
  toYears :: a -> Float -> Float

data Planet = Mercury
            | Venus
            | Earth
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune

instance Orbital Planet where
  toYears p s = (/) earthYears $ case p of
    Earth   -> 1
    Mercury -> 0.2408467
    Venus   -> 0.61519726
    Mars    -> 1.8808158
    Jupiter -> 11.862615
    Saturn  -> 29.447498
    Uranus  -> 84.016846
    Neptune -> 164.79132
    where earthYears = s / 31557600

ageOn :: Planet -> Float -> Float
ageOn = toYears
