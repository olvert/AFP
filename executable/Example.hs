module Main where

import TurtleGraphics
import Turtle
import TurtleTextual

-- main = runTextual $ spiral 10 10
-- main = runGraphical $ limited 100 $ spiralForever 10 90
main = runTextual $ dummy
-- | Returns a program that draws a finite spiral.
spiral :: Double -> Double -> Program
spiral size angle | size > 100 = idle
                  | otherwise = forward size >*>
                                right angle >*>
                                spiral (size + 2) angle

-- | Returns a program that draws a infinite spiral.
spiralForever :: Double -> Double -> Program
spiralForever size angle = forever $ forward size >*>
                                     right angle >*>
                                     spiralForever (size + 2) angle

-- | Returns a program that draws a finite spiral
-- followed by an infinite spiral.
spiralThenSpiral :: Double -> Double -> Program
spiralThenSpiral size angle = spiral size angle >*>
                              spiralForever size angle


-- DUMMY PROGRAMS

dummy :: Program
dummy = turnAndMove 0 20 >*> turnAndMove 0 20 >*> (tnms (pi/4) <|> tnms (-pi/4))
  where tnms d = turnAndMove d 20 >*> move 40

turnAndMove :: Double -> Double -> Program
turnAndMove r d = turn r >*> move d
