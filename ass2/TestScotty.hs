{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Scotty
import Data.Monoid
import Data.Text.Lazy hiding (filter, length)
import WebForm
import Replay

-- | Main run function for Scotty instance.
main :: IO ()
main = scotty 3000 $ do
    get "/" (runWeb example)
    post "/" (runWeb example)

-- | Simple example form application.
example :: Web String
example = do
  let qi1 = ("people", Num)
      qi2 = ("color", String)
      qi3 = ("calc", Num)
      as = [(qi1, "10"), (qi2, "magenta"), (qi3, "1361")]
  guesses <- ask [
    (qi1, "How many million people lives in Sweden?"),
    (qi2, "What color do you get if you blend blue and red light?"),
    (qi3, "What is the next prime number after 1337?")]
  let (r, w) = getScore as guesses
      result = "You scored " ++ show r ++ "/" ++ show (r + w)
  return result

-- | Calculates the result of form answers.
getScore :: Answers -> Form -> (Int, Int)
getScore as qs = (length (filter (== True) t), length (filter (== False) t))
  where t = [ lookup k as == Just vq | (k, vq) <- qs ]
