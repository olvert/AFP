module Main where

import TurtleGraphics
import Turtle
import TurtleTextual
import Data.Word

main = runGraphical tree

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

-- | Binary spectral tree 
tree :: Program
tree = penColor (139, 69, 19) >*> move 100 >*> 
       branch 9 100 100 0.7 0.7 (-pi/12) (pi/4)

-- | Branches of a spectral tree based on itterations, length, 
-- decreasing factor and angle 
branch :: Int -> Double -> Double -> 
          Double -> Double -> Double -> Double -> Program
branch n sizeL sizeR factorL factorR radL radR
  | n <= 0    = idle
  |otherwise = branch' sizeL radL <|> branch' sizeR radR
    where branch' size rad = turn rad >*> forward size >*>
                             branch (n-1) (sizeL * factorL) (sizeR * factorR)
                                    factorL factorR radL radR
