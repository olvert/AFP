{-# LANGUAGE GADTs #-}

-- | EDSL for Turtle (similiar to Logo programming)
module Turtle (

  -- * Types
  Program (Idle, Die, PenUp, PenDown, PenColor, Move, Turn, Chain, Parallel)
  , Turtle (Dead, Alive)
  , Pen (Pen)
  , Time, Pos, Dir, Color

  -- * Constructors
  , idle, move, turn, forward, backward, right, left, die, penColor, penUp, penDown

  -- * Combinators
  , times, forever, limited, lifespan, (>*>), (<|>)

  ) where

import Data.Word
import Data.Maybe

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
  Chain    :: Program -> Program -> Program
  Parallel :: Program -> Program -> Program


-- * Primitive Operations
-- | A program that does nothing.
idle :: Program
idle = Idle

-- | Kills the turtle making it unable to perform any more actions.
die :: Program
die = Die

-- | Stops the turtle from drawing
penUp :: Program
penUp = PenUp

-- | Makes the turtle draw
penDown :: Program
penDown = PenDown

-- | Changes the color the turtle draws with
penColor :: Color -> Program
penColor = PenColor

-- | Moves turtle a number of steps.
move :: Double -> Program
move = Move

-- | Rotates the turtle n degrees.
turn :: Double -> Program
turn = Turn

-- | Sequencing operator used to run programs one after another.
(>*>) :: Program -> Program -> Program
(>*>) = Chain

-- | Parallel operator used to run programs in parallel.
(<|>) :: Program -> Program -> Program
(<|>) = Parallel

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

-- | Repeats a Program forever.
forever :: Program -> Program
forever p = foldr (>*>) p $ repeat p

limited :: Time -> Program -> Program
limited t p = snd $ limited' t p


-- | Runs a program for a limited amount of time.
-- TODO: Improve this one to make it cut of program instead of filling w Idle
limited' :: Time -> Program -> (Time, Program)
limited' t (Chain p1 p2) = case limited' t p1 of
                            (-1, p1') ->  (-1, p1')
                            (t', p1') ->  let (t'', p2') = limited' t' p2 in
                                          (t'', p1' >*> p2')
limited' t (Parallel p1 p2) = (t, p1' <|> p2')
  where (_, p1') = limited' t p1
        (_, p2') = limited' t p2
limited' t p | limit t p = (-1, Idle)
             | otherwise = (t - cost p, p)


-- | Helper function for limited.
limit :: Time -> Program -> Bool
limit t p = (t - cost p) < 0

-- | Kills the turtle after a specified amount of time.
lifespan :: Time -> Program -> Program
lifespan t p = limited t p >*> Die

-- * Helper Functions

cost :: Program -> Time
cost (Chain p1 p2)    = undefined -- ost p1 + cost p2
cost (Parallel p1 p2) = undefined -- max (cost p1) (cost p2)
cost _p               = 1
