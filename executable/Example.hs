module Main where

import TurtleGraphics
import Turtle
-- import TurtleExtras

main = runTextual coolExample

coolExample = times 3 (right 5) >*> idle

spiral :: Double -> Double -> Program a
spiral size _angle | size > 100 = die
spiral size angle  | otherwise = (forward size) >*> 
                                 (right angle) >*> 
                                 (spiral (size + 2) angle)

spiralForever :: Double -> Double -> Program a
spiralForever size angle = forever $ (forward size) >*> 
                                     (right angle) >*> 
                                     (spiralForever (size + 2) angle)

-- spiralForeverLimited :: Double -> Double -> Program a
-- spiralForeverLimited size angle = limited 73 $ spiralForever size angle

spiralThenSpiral :: Double -> Double -> Program a
spiralThenSpiral size angle = (spiral size angle) >*> (spiralForever size angle)