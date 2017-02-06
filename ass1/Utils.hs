module Utils where

everyOther :: [a] -> [a] -> [a]
everyOther xs     []     = xs
everyOther []     ys     = ys
everyOther (x:xs) (y:ys) = x : y : everyOther xs ys
