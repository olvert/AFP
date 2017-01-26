{-# LANGUAGE GADTs #-}
-- | proper module documentation here
module Turtle (
  -- * The turtle type(s)
  -- Non-exhaustive list of possible types: Turtle, Program, Action, Operation ...
  Program, Turtle

  -- * Primitive operations
  , idle, forward, backward, right, left, die

  -- * Derived operations
  , times, forever, limited, lifespan, (>*>)

  -- * Run functions
  , runTextual, run
  -- ...

  ) where

import Prelude hiding (Right, Left)
import Control.Applicative (Applicative(..))
import Control.Monad (liftM, ap)

data Turtle = Turtle (Double, Double) (Double, Double)

-- | Description of your type here...
--
--   You can use newtype instead of data if you wish.
data Program a where
  -- Constructors
  Idle     :: Program a
  Die      :: Program a
  PenUp    :: Program a
  PenDown  :: Program a
  Forward  :: Double -> Program a
  Backward :: Double -> Program a
  Right    :: Double -> Program a
  Left     :: Double -> Program a
  -- Combinators
  Times    :: Int -> Program a -> Program a
  Forever  :: Program a -> Program a
  Limited  :: Int -> Program a -> Program a
  Lifespan :: Int -> Program a -> Program a
  Chain    :: Program a -> Program a -> Program a
  -- Monadic oprations
  Return   :: a -> Program a
  Bind     :: Program a -> (a -> Program b) -> Program b


idle :: Program a
idle = Idle

forward :: Double -> Program a
forward = Forward

backward :: Double -> Program a
backward = Backward

right :: Double -> Program a
right = Right

left :: Double -> Program a
left = Left

times :: Int -> Program a -> Program a
times = Times

forever :: Program a -> Program a
forever = Forever

limited  :: Int -> Program a -> Program a
limited = Limited

die :: Program a
die = Die

lifespan :: Int -> Program a -> Program a
lifespan = Lifespan

(>*>) :: Program a -> Program a -> Program a
(>*>) = Chain

runTextual :: Program a -> IO() 
runTextual Idle          = putStrLn $ "Idle."
runTextual Die           = putStrLn $ "Die."
runTextual (Forward x)   = putStrLn $ "Move forward " ++ show x ++ " units."
runTextual (Backward x)  = putStrLn $ "Move backward " ++ show x ++ " units."
runTextual (Right d)     = putStrLn $ "Turn right " ++ show d ++ " degrees."
runTextual (Left d)      = putStrLn $ "Turn left " ++ show d ++ " degrees."
runTextual (Times i p)   = sequence_ $ replicate i $ runTextual p 
runTextual (Forever p)   = sequence_ $ repeat $ runTextual p
runTextual (Limited i p) = undefined 
runTextual (Chain p1 p2) = sequence_ [runTextual p1, runTextual p2]


run :: Program a -> IO()
run = undefined
-- run Idle         = putStr $ "Idle."
-- run (Forward x)  = putStr $ "Move forward " ++ show x ++ " units."
-- run (Backward x) = putStr $ "Move backward " ++ show x ++ " units."
-- run (Right d)    = putStr $ "Turn right " ++ show d ++ " degrees."
-- run (Left d)     = putStr $ "Turn left " ++ show d ++ " degrees."
-- run (Times i p)  = sequence_ $ replicate i $ runTextual p 
-- run (Forever p)  = sequence_ $ repeat $ runTextual p

instance Monad Program where
  return = Return
  (>>=)  = Bind

instance Functor Program where 
  fmap = liftM

instance Applicative Program where 
  pure  = return
  (<*>) = ap