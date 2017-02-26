{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Scotty
import Data.Monoid
import Data.Text.Lazy hiding (filter, length)
import WebForm
import Replay

main :: IO ()
main = scotty 3000 $ do
    get "/" (runWeb example)
    post "/" (runWeb example)


example :: Web ()
example = do
  let as = [("people", "10"), ("color", "magenta"), ("calc", "1361")]
  guesses <- ask [
    ("people", "How many million people lives in Sweden?"), 
    ("color", "What color do you get if you blend blue and red light?"),
    ("calc", "What is the next prime number after 1337?")]
  let (r, w) = getScore as guesses 
  io (putStrLn ("You scored " ++ show r ++ "/" ++ show (r + w)))
  ask [("result", "You scored " ++ show r ++ "/" ++ show (r + w))]
  return ()

getScore :: [(String, String)] -> [(String, String)] -> (Int, Int)
getScore as qs = (length (filter (== True) t), length (filter (== False) t)) 
  where t = [ lookup k as == Just vq | (k, vq) <- qs ]