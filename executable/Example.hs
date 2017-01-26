module Main where

import TurtleGraphics
import Turtle
-- import TurtleExtras

main = runTextual $ spiral 10 10

-- | Returns a program that draws a finite spiral.
spiral :: Double -> Double -> Program a
spiral size angle | size > 100 = die
                  | otherwise = forward size >*>
                                right angle >*>
                                spiral (size + 2) angle

-- | Returns a program that draws a infinite spiral.
spiralForever :: Double -> Double -> Program a
spiralForever size angle = forever $ forward size >*>
                                     right angle >*>
                                     spiralForever (size + 2) angle

-- | Returns a program that draws a finite spiral
-- followed by an infinite spiral.
spiralThenSpiral :: Double -> Double -> Program a
spiralThenSpiral size angle = spiral size angle >*>
                              spiralForever size angle
