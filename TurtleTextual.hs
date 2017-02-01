-- | A run fuction printing the actions of a program in sequential order.
module TurtleTextual where

import Turtle

-- | Observes a program and prints the actions in sequential order.
runTextual :: Program -> IO()
runTextual Idle          = putStrLn "Idle."
runTextual Die           = putStrLn "Die."
runTextual (Move x)      | x >= 0    = putStrLn $ "Move forward " ++ show x ++ " units."
                         | otherwise = putStrLn $ "Move backwards " ++ show x ++ " units."
runTextual (Turn d)      | d < 0     = putStrLn $ "Turn right " ++ show d ++ " degrees."
                         | otherwise = putStrLn $ "Turn left " ++ show d ++ " degrees."
--runTextual (Limited i p) = undefined
runTextual (Chain p1 p2) = sequence_ [runTextual p1, runTextual p2]
