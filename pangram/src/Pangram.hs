module Pangram (isPangram) where

import Data.List
import Data.Char

isPangram :: String -> Bool
isPangram =  (== ['a'..'z']) . nub . alphagram

alphagram, lowercase :: String -> String
alphagram str = (sort . lowercase . filter isAlpha) str
lowercase = map toLower
