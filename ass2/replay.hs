{-# LANGUAGE GADTs, FlexibleInstances #-}
module Replay (
  Replay, Trace,

  io, ask,

  emptyTrace, addAnswer,

  run
  ) where

import Control.Applicative
import Control.Monad (liftM, ap)
import Data.Time


-- Types
data Replay q r a where
  IO     :: (Show a, Read a) => IO a -> Replay q r a
  Ask    :: q -> Replay q r r
  Return :: a -> Replay q r a
  Bind   :: Replay q r a -> (a -> Replay q r b) -> Replay q r b

type Trace r = [Item r]

data Item r = Answer r | Result String
  deriving (Show, Read)

-- Operations
instance Monad (Replay q r) where
  return = Return
  (>>=)  = Bind

instance Functor (Replay q r) where
  fmap = liftM

instance Applicative (Replay q r) where
  pure  = return
  (<*>) = ap


io  :: (Show a, Read a) => IO a -> Replay q r a
io =  IO

ask :: q -> Replay q r r
ask = Ask

emptyTrace :: Trace r
emptyTrace = []

addAnswer  :: Trace r -> r -> Trace r
addAnswer t a = t ++ [Answer a]

addResult  :: Show s => Trace r -> s -> Trace r
addResult t s = t ++ [Result $ show s]





-- run :: Replay q r a -> Trace r -> IO (Either (q, Trace r) a) -- typechecks but does not do what we want..
-- run (IO a) []             = do  a' <- a
--                                 return $ Right a'
-- run (IO a) (Result t:ts)  = return $ Right $ read t
-- run (Ask q) (Answer a:ts) = return $ Right a
-- run (Ask q) t             = return $ Left (q, t)
-- run (Return a) t          = return (Right a)
-- run (Bind r f) t          = do r' <- run r t
--                                case r' of
--                                  (Right a)      -> run (f a) t
--                                  (Left (q, t')) -> return $ Left (q, t')
-- run _ _                   = error "No match in pattern"



run :: Replay q r a -> Trace r -> IO (Either (q, Trace r) a)
run r t = convert $ run' r t

run' :: Replay q r a -> Trace r -> IO (Either (q, Trace r) (a, Item r))
run' (IO a) []             = do a' <- a
                                return $ Right (a', Result ( show a'))
run' (IO a) (Result t:ts)  = return $ Right (read t, Result (show t))
run' (Ask q) (Answer a:ts) = return $ Right (a, Answer a)
run' (Ask q) t             = return $ Left (q, t)
run' (Return a) t          = return $ Right (a, Result "")
run' (Bind r f) t          = do r' <- run' r t
                                case r' of
                                  (Right (a, Result r)) -> run' (f a) (addResult t r)
                                  (Right (a, Answer r)) -> run' (f a) (addAnswer t r)
                                  (Left (q, t'))  -> return $ Left (q, t')
run' _ _                   = error "No match in pattern"

convert :: IO (Either (q, Trace r) (a, Item r)) -> IO (Either (q, Trace r) a)
convert e = do e' <- e
               case e' of
                 (Left (q, t))  -> return $ Left (q, t)
                 (Right (a, i)) -> return $ Right a


example :: Replay String String Int
example = do
  t0 <- io getCurrentTime
  io (putStrLn "Hello!")
  age <- ask "What is your age?"
  io (putStrLn ("You are " ++ age))
  name <- ask "What is your name?"
  io (putStrLn (name ++ " is " ++ age ++ " years old"))
  t1 <- io getCurrentTime
  io (putStrLn ("Total time: " ++ show (diffUTCTime t1 t0)))
  return (read age)