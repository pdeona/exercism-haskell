module Bob (responseFor) where

import Data.Char

data Greeting
  = Yelled String
  | Spoken String

responseFor :: String -> String
responseFor xs = respond . identify $ xs
  where
    identify :: String -> Greeting
    identify ys = case takeWhile isUpper $ filter isAlpha ys' of
      ""     -> Spoken ys'
      (_:"") -> Spoken ys'
      (_:_)  -> Yelled ys'
      where ys' = dropTrailingSpaces "" $ dropWhile isSpace ys
    respond :: Greeting -> String
    respond (Spoken "") = "Fine. Be that way!"
    respond ys
      | ys `endsWith` '?' = respondToQuestion ys
    respond (Yelled _)    = "Whoa, chill out!"
    respond (Spoken _)    = "Whatever."

    respondToQuestion (Yelled _) = "Calm down, I know what I'm doing!"
    respondToQuestion (Spoken _) = "Sure."

endsWith :: Greeting -> Char -> Bool
endsWith gs c = gEnd gs == c

gEnd :: Greeting -> Char
gEnd (Spoken xs) = last xs
gEnd (Yelled xs) = last xs

dropTrailingSpaces :: String -> String -> String
dropTrailingSpaces _ "" = ""
dropTrailingSpaces hd (x:xs)
        | isSpace x = dropTrailingSpaces (x:hd) xs
        | null hd   = x : dropTrailingSpaces "" xs
        | otherwise = reverse hd ++ x : dropTrailingSpaces "" xs
