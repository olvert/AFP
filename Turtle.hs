{-# LANGUAGE GADTs #-}

-- | EDSL for Turtle (similiar to Logo programming)
module Turtle (

  -- * Types
  Program, Color, Time

  -- * Constructors
  , idle, forward, backward, right, left, die

  -- * Combinators
  , times, forever, limited, lifespan, (>*>)

  -- * Run functions
  , runTextual

  ) where

-- | Type for pen color with three doubles representing a RGB value.
type Color = (Double, Double, Double)

-- | Type for abstract time used by limited and lifespan combinators.
type Time = Int

-- | Represents the position of a turtle through an x and y coordinate.
type Pos = (Int, Int)

-- | Represents the direction of a turtle through an x and y coordinate.
type Dir = (Int, Int)

-- | Pen type containing color and bool telling whether pen is up or down.
data Pen = Color Bool

-- | Turtle type containing all relevant state information for a turtle.
data Turtle = Dead | Alive Pos Dir

-- | A single instruction or a set of instructions that defines
-- the behaviour of the turtle.
data Program a where

  -- Constructors
  Idle     :: Program a
  Die      :: Program a
  PenUp    :: Program a
  PenDown  :: Program a
  PenColor :: Color  -> Program a
  Move     :: Double -> Program a
  Turn     :: Double -> Program a

  -- Combinators
  Limited  :: Time -> Program a -> Program a
  Lifespan :: Time -> Program a -> Program a
  Chain    :: Program a -> Program a -> Program a


-- | A program that does nothing.
idle :: Program a
idle = Idle

-- | Kills the turtle making it unable to perform any more actions.
die :: Program a
die = Die

-- | Runs a program for a limited amount of time.
limited  :: Time -> Program a -> Program a
limited = Limited

-- | Kills the turtle after a specified amount of time.
lifespan :: Time -> Program a -> Program a
lifespan = Lifespan

-- | Sequencing operator used to run programs one after antother.
(>*>) :: Program a -> Program a -> Program a
(>*>) = Chain


-- * Derived Operations
-- | Moves turtle forward a number of steps.
forward  :: Double -> Program a
forward = Move

-- | Moves turtle backwards a number of steps.
backward :: Double -> Program a
backward d = Move (-d)

-- | Turns the angle of the turtle to the right.
right :: Double -> Program a
right d = Turn (-d)

-- | Turns the angle of the turtle to the left.
left :: Double -> Program a
left = Turn

-- | Repeats a program a number of times.
times :: Int -> Program a -> Program a
times n p = foldr (>*>) p $ replicate (n-1) p

-- | Repeats a program forever.
forever :: Program a -> Program a
forever p = foldr (>*>) p $ repeat p

-- | Observes a program and prints the actions in sequential order.
runTextual :: Program a -> IO()
runTextual Idle          = putStrLn "Idle."
runTextual Die           = putStrLn "Die."
runTextual (Move x)      | x >= 0    = putStrLn $ "Move forward " ++ show x ++ " units."
                         | otherwise = putStrLn $ "Move backwards " ++ show x ++ " units."
runTextual (Turn d)      | d < 0     = putStrLn $ "Turn right " ++ show d ++ " degrees."
                         | otherwise = putStrLn $ "Turn left " ++ show d ++ " degrees."
runTextual (Limited i p) = undefined
runTextual (Chain p1 p2) = sequence_ [runTextual p1, runTextual p2]
