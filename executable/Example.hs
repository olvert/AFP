module Main where

import TurtleGraphics
import Turtle
import TurtleTextual
import Data.Word

-- main = runTextual $ spiral 10 10
-- main = runGraphical $ limited 100 $ spiralForever 10 90
main = runGraphical $ tree
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
dummy = turnAndMove 0 20 >*> turnAndMove 0 20 >*> (tnms (pi/12) <|> tnms (-pi/4))
  where tnms d = turnAndMove d 20 >*> move 40

turnAndMove :: Double -> Double -> Program
turnAndMove r d = turn r >*> move d


-- | Tree
tree :: Program
tree =  penColor (139, 69, 19) >*> move 100 >*> branch 10 100 100 0.7 0.7 (-pi/12) (pi/4)

branch :: Int -> Double -> Double -> Double -> Double -> Double -> Double -> Program
branch n sizeL sizeR factorL factorR radL radR 
  | n <= 0    = idle
  |otherwise = branch' sizeL radL <|> branch' sizeR radR
    where branch' size rad = turn rad >*> forward size >*> 
                             branch (n-1) (sizeL * factorL) (sizeR * factorR)
                                    factorL factorR radL radR 