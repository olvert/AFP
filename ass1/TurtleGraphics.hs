-- | A graphical run function for the turtle DSL. 
module TurtleGraphics (runGraphical) where

import Turtle
import Utils
import Graphics.HGL
import Data.Word

-- | Draws the turtle program.
runGraphical :: Program -> IO ()
runGraphical p = runGraphics $ do
    w <- openWindowEx "Turtle!" Nothing (1000, 1000) DoubleBuffered (Just 1)
    drawInWindow w (polygon [(0,0),(0,1000),(1000,1000),(1000,0)])
    let (_t, gs) = runProgram p defaultTurtle
    onTick w gs
    getKey w >> return ()

-- | Generates a list of Graphics instructions together with the corresponding
-- turtle representation.
runProgram :: Program -> Turtle -> (Turtle, [Graphic])
runProgram _ Dead             = (Dead, [])
runProgram Idle t             = (t, [])
runProgram Die t              = (Dead, [])
runProgram PenUp t            = (runPenDown t False, [])
runProgram PenDown t          = (runPenDown t True, [])
runProgram (PenColor c) t     = (runPenColor t c, [])
runProgram (Move d) t         = turtleLine d t
runProgram (Turn d) t         = (rotate t d, [])
runProgram (Chain p1 p2) t    = (t2, g1 ++ g2)
  where (t1, g1) = runProgram p1 t
        (t2, g2) = runProgram p2 t1
runProgram (Parallel p1 p2) t = (t, everyOther g1 g2)
  where (_t1, g1) = runProgram p1 t
        (_t2, g2) = runProgram p2 t

-- | Changes the turtles pen color.
runPenColor :: Turtle -> Turtle.Color -> Turtle
runPenColor (Alive p d (Pen c b)) c' = (Alive p d (Pen c' b))

-- | Changes the turtles pen state.
runPenDown :: Turtle -> Bool -> Turtle
runPenDown (Alive p d (Pen c b)) b' = (Alive p d (Pen c b'))

-- | Moves the turtle and draws a line along the path.
turtleLine :: Double -> Turtle -> (Turtle, [Graphic])
turtleLine d Dead = (Dead, []) -- Why is this needed?
turtleLine d (Alive (x, y) (dirx, diry) (Pen c b)) = (t', action)
  where newPos = (x + round (dirx * d), y + round (diry * d))
        t'     = Alive newPos (dirx, diry) (Pen c b)
        action = if b then [withRGB (toHglRgb c) $ line (x, y) newPos] else []

-- | Mapping from Turtle.Color representation to HGL Color representation.
toHglRgb :: Turtle.Color -> RGB
toHglRgb (r,g,b) = RGB r g b

-- | Rotates the turtels direction vector.
rotate :: Turtle -> Double -> Turtle
rotate Dead _d = Dead
rotate (Alive pos (x,y) pen) d = Alive pos (normalize newDir) pen
  where newDir = (x * cos d - y * sin d, x * sin d + y * cos d)

-- | Normalize the direction vector.
normalize :: Dir -> Dir
normalize (x,y) = (x / l, y / l)
  where l = sqrt(x^2 + y^2)

-- | Draws elements in the windows from a list on the windows tick function. 
onTick :: Window -> [Graphic] -> IO ()
onTick w []      = return ()
onTick w (x:xs)  = do
  getWindowTick w
  drawInWindow w x
  onTick w xs

defaultPen :: Turtle.Pen
defaultPen = Pen (0, 0, 0) True

defaultTurtle :: Turtle
defaultTurtle = Alive (500,500) (0,-1) defaultPen
