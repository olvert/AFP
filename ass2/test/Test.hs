module Main where

import Data.IORef
import Replay
import System.Exit

-- | Runs the test suite for the replay library
main :: IO ()
main = do
  results <- runTests
  if and results
    then return ()
    else exitFailure

-- | Programs are parameterised over a 'tick' action.
--   Questions are () and answers are integers.
type Program = IO () -> Replay () Int Int

-- | A result is a pair of the final result of the program
--   and the number of 'ticks' executed.
type Result  = (Int, Int)
type Input   = [Int]

-- | A test case collects a program and a list of answers together
--   with its expected result.
data TestCase = TestCase
  { testName    :: String
  , testInput   :: Input
  , testResult  :: Result
  , testProgram :: Program
  }

-- | Running a program.
runProgram :: Program -> Input -> IO Result
runProgram p inp = do
    counter <- newIORef 0
    let tick = modifyIORef counter (+1)
    x <- play (p tick) emptyTrace inp
    n <- readIORef counter
    return (x, n)
  where
    play prog t inp = do
      r <- run prog t
      case r of
        Right x      -> return x
        Left (_, t') -> case inp of
          []       -> error "too few inputs"
          a : inp' -> play prog (addAnswer t' a) inp'

-- | Checking a test case. Compares expected and actual results.
checkTestCase :: TestCase -> IO Bool
checkTestCase (TestCase name i r p) = do
  putStr $ name ++ ": "
  r' <- runProgram p i
  if r == r'
    then putStrLn "ok" >> return True
    else putStrLn ("FAIL: expected " ++ show r ++
                  " instead of " ++ show r')
         >> return False


-- | List of interesting test cases.
testCases :: [TestCase]
testCases =
  [ TestCase
    { testName    = "default test"
    , testInput   = [3,4]
    , testResult  = (8, 1)
    , testProgram = \tick -> do
        io tick
        a <- ask () -- should be 3
        b <- io (return 1)
        c <- ask () -- should be 4
        return (a + b + c)
    } ,
    TestCase
    { testName    = "empty test"
    , testInput   = []
    , testResult  = (0, 0)
    , testProgram = \tick -> return 0
    } ,
    TestCase
    { testName    = "only io test"
    , testInput   = []
    , testResult  = (1, 2)
    , testProgram = \tick -> do
        io tick
        a <- io (return 3)
        io tick
        b <- io (return 4)
        return (b - a)
    } ,
    TestCase
    { testName    = "only ask test"
    , testInput   = [1,2,3]
    , testResult  = (6, 0)
    , testProgram = \tick -> do
        a <- ask ()
        b <- ask ()
        c <- ask ()
        return (a + b + c)
    } ,
    TestCase
    { testName    = "long test"
    , testInput   = [1,2,3,4,5]
    , testResult  = (22, 3)
    , testProgram = \tick -> do
        a <- ask ()
        io tick
        b <- io (return 5)
        io tick
        c <- io (return 1)
        d <- ask ()
        e <- ask ()
        io tick
        f <- ask ()
        g <- io (return 1)
        h <- ask ()
        return (a + b + c + d + e + f + g + h)
    } ,
    TestCase
    { testName    = "monad laws 1"
    , testInput   = [3,4]
    , testResult  = (8, 1)
    , testProgram = \tick -> do
        return undefined -- XXX: doesn't advance the trace position
        io tick
        a <- ask ()
        b <- io (return 1)
        c <- ask ()
        return (a + b + c)
    } ,
    TestCase
    { testName    = "monad laws 2"
    , testInput   = [3,4]
    , testResult  = (8, 1)
    , testProgram = \tick -> do
        a <- do io tick -- XXX: advances the trace position once
                ask ()  -- XXX: advances the trace position a second time
        b <- io (return 1)
        c <- ask ()
        return (a + b + c)
    }
  ]

-- | Running all the test cases.
runTests = mapM checkTestCase testCases
