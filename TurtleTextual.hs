-- | A run fuction printing the actions of a program in sequential order.
module TurtleTextual where

import Turtle
import Utils

-- | Observes a program and prints the actions in sequential order.
runTextual :: Program -> IO ()
runTextual p = putStrLn $ unlines $ runProgram p "A"

-- | Observes a program and returns the actions in sequential as strings.
runProgram :: Program -> String -> [String]
runProgram Idle f             = lst "Idle."
runProgram Die f              = lst "Die."
runProgram (Move x) f         | x >= 0    = form f $ "Move forward " ++ show x ++ " units."
                              | otherwise = form f $ "Move backwards " ++ show x ++ " units."
runProgram (Turn d) f         | d < 0     = form f $ "Turn right " ++ show d ++ " degrees."
                              | otherwise = form f $ "Turn left " ++ show d ++ " degrees."
runProgram (Chain p1 p2) f    = runProgram p1 f ++ runProgram p2 f
runProgram (Parallel p1 p2) f = everyOther (runProgram p1 f1) (runProgram p2 f2)
  where (f1, f2) = (f++"A", f++"B")

-- | Formats a string to fit runTextual.
form :: String -> String -> [String]
form f s = lst $ concat [f, "\t\t: ", s]

-- | Takes an element and returns a single element list.
lst :: a -> [a]
lst x = [x]
