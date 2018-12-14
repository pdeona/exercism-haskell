module Strain (keep, discard) where

keep, discard :: (a -> Bool) -> [a] -> [a]
keep p xs = [x | x <- xs, p x]
discard p = keep (not . p)
