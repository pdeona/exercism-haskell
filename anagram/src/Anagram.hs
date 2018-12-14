module Anagram (anagramsFor) where

import Data.List
import Data.Char

anagramsFor :: String -> [String] -> [String]
anagramsFor xs xss = [x | x <- xss, notWord x, ag == alphagram x ]
  where
    notWord = (/= lowercase xs) . lowercase
    ag = alphagram xs

alphagram :: String -> String
alphagram str = (sort . lowercase) str

lowercase :: String -> String
lowercase = map toLower

