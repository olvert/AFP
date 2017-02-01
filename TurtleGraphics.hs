-- | A graphical run function for the turtle DSL
module TurtleGraphics (runGraphical) where

import Turtle

import Graphics.HGL
import Data.Word

runGraphical :: Program -> IO ()
runGraphical p = runGraphics $ do
    w <- openWindowEx "Turtle!" Nothing (300, 300) DoubleBuffered (Just 1000)
    drawInWindow w (polygon [(0,0),(0,300),(300,300),(300,0)])
    onTick w [ withColor Graphics.HGL.Red   $ line  (100, 200) (200, 100)
             , withColor Graphics.HGL.Green $ line  (100, 100) (200, 200)
             , withColor Blue  $ line  (150, 100) (150, 200)
             , withColor Black $ line  (100, 150) (200, 150)
             ]
    getKey w >> return ()


-- Parse error in pattern... Are the runFunction suppose to be here?

runProgram :: Program -> Turtle -> (Turtle, [Graphic])
runProgram (idle) t = (t, [])
runProgram (die) t = (Dead, [])
-- runProgram (move d) t = (t, turtleLine d t)
-- runProgram (turn) t = (rotate t, [])
-- runProgram ((>*>) p1 p2) t = (t2, g1 ++ g2)
--   where (t1, g1) = runProgram p1 t
--         (t2, g2) = runProgram p2 t1

turtleLine :: Double -> Turtle -> (Turtle, Graphic)
turtleLine d (Alive (x, y) (dirx, diry) pen) =
  (t', withRGB (toHglRgb c) $ line (x, y) newPos)
  where newPos = (x + round (dirx * d), y + round (diry * d))
        t'     = Alive newPos (dirx, diry) pen
        (Pen c b) = pen

toHglRgb :: (Int, Int, Int) -> RGB
toHglRgb (r,g,b) = RGB (fromIntegral r) (fromIntegral g) (fromIntegral b)

rotate :: Turtle -> Double -> Turtle
rotate (Alive pos (x,y) pen) d = Alive pos newDir pen
  where newDir = (x * cos d - y * sin d, x * sin d + y * cos d)

normalize :: Dir -> Dir
normalize (x,y) = (x / l, y / l)
  where l = sqrt(x^2 + y^2)

onTick :: Window -> [Graphic] -> IO ()
onTick w []      = return ()
onTick w (x:xs)  = do
  getWindowTick w
  drawInWindow w x
  onTick w xs
