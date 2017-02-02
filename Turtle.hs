{-# LANGUAGE GADTs #-}

-- | EDSL for Turtle (similiar to Logo programming)
module Turtle (

  -- * Types
  Program (Idle, Die, PenUp, PenDown, PenColor, Move, Turn, Lifespan, Chain)
  , Turtle (Dead, Alive)
  , Pen (Pen)
  , Time, Pos, Dir, Color

  -- * Constructors
  , idle, move, turn, forward, backward, right, left, die

  -- * Combinators
  , times, forever, limited, lifespan, (>*>)

  ) where

import Data.Word

-- | Type for pen color representing a RGB value.
type Color = (Word8, Word8, Word8)

-- | Type for abstract time used by limited and lifespan combinators.
type Time = Int

-- | Represents the position of a turtle through an x and y coordinate.
type Pos = (Int, Int)

-- | Represents the direction of a turtle through an x and y coordinate.
type Dir = (Double, Double)

-- | Pen type containing color and bool telling whether pen is up or down.
data Pen = Pen Color Bool

-- | Turtle type containing all relevant state information for a turtle.
data Turtle = Dead | Alive Pos Dir Pen

-- | A single instruction or a set of instructions that defines
-- the behaviour of the turtle.
data Program where

  -- Constructors
  Idle     :: Program
  Die      :: Program
  PenUp    :: Program
  PenDown  :: Program
  PenColor :: Color  -> Program
  Move     :: Double -> Program
  Turn     :: Double -> Program

  -- Combinators
  Limited  :: Time -> Program -> Program
  Lifespan :: Time -> Program -> Program
  Chain    :: Program -> Program -> Program


-- * Primitive Operations
-- | A program that does nothing.
idle :: Program
idle = Idle

-- | Kills the turtle making it unable to perform any more actions.
die :: Program
die = Die

-- | Moves turtle a number of steps.
move :: Double -> Program
move = Move

-- | Rotates the turtle n degrees.
turn :: Double -> Program
turn = Turn

-- | Runs a program for a limited amount of time.
limited :: Time -> Program -> Program
limited = Limited

-- | Kills the turtle after a specified amount of time.
lifespan :: Time -> Program -> Program
lifespan = Lifespan

-- | Sequencing operator used to run programs one after antother.
(>*>) :: Program -> Program -> Program
(>*>) = Chain


-- * Derived Operations
-- | Moves turtle forward a number of steps.
forward  :: Double -> Program
forward = Move

-- | Moves turtle backwards a number of steps.
backward :: Double -> Program
backward d = Move (-d)

-- | Rotates the turtle n degrees to the right.
right :: Double -> Program
right d = Turn (-d)

-- | Rotates the turtle n degrees to the left.
left :: Double -> Program
left = Turn

-- | Repeats a Program number of times.
times :: Int -> Program -> Program
times n p = foldr (>*>) p $ replicate (n-1) p

-- | Repeats a program forever.
forever :: Program -> Program
forever p = foldr (>*>) p $ repeat p
