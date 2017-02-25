{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Scotty
import Data.Monoid
import Data.Text.Lazy
import WebForm
import Replay

main :: IO ()
main = scotty 3000 $ do
    get "/" (runWeb example)
    post "/" (runWeb example)

example :: Web ()
example = do
  io (putStrLn "Hello!")
  age <- ask [("age", "What is your age?"), ("leo", "Who is Leo?")]
  return ()
