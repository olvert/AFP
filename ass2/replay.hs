{-# LANGUAGE GADTs, FlexibleInstances #-}

-- | Module containing functions and constructors for creating replay
-- applications that can be executed using traces from previous executions.
module Replay (
  Replay, Trace, Item (Result, Answer),

  io, ask,

  emptyTrace, addAnswer,

  run
  ) where

import Control.Applicative
import Control.Monad (liftM, ap)
import Data.Time


-- * Types

-- | Main type for producing replay applications.
data Replay q r a where
  IO     :: (Show a, Read a) => IO a -> Replay q r a
  Ask    :: q -> Replay q r r
  Return :: a -> Replay q r a
  Bind   :: Replay q r a -> (a -> Replay q r b) -> Replay q r b

-- | List representing appliction execution trace.
type Trace r = [Item r]

-- | Representing a single element in application trace.
data Item r = Answer r | Result String
  deriving (Show, Read)

-- Internal type for routing information in interp function.
newtype T q r a = T (Either q a, Trace r, Trace r)

-- * Operations
instance Monad (Replay q r) where
  return = Return
  (>>=)  = Bind

instance Functor (Replay q r) where
  fmap = liftM

instance Applicative (Replay q r) where
  pure  = return
  (<*>) = ap


-- | Generates replay application from an IO operation.
io  :: (Show a, Read a) => IO a -> Replay q r a
io =  IO

-- | Generates replay application with a question as argument.
ask :: q -> Replay q r r
ask = Ask

-- | Returns an empty trace.
emptyTrace :: Trace r
emptyTrace = []

-- | Takes a trace and appends an answer.
addAnswer  :: Trace r -> r -> Trace r
addAnswer t a = t ++ [Answer a]

-- | Main function used for evaluating replay applications.
-- 'it' stands from input trace and 'ot' for output trace
run :: Replay q r a -> Trace r -> IO (Either (q, Trace r) a)
run r t = do
  r' <- interp r t []
  case r' of
    T (Right a, it, ot) -> return (Right a)
    T (Left q, it, ot)  -> return (Left (q,ot))

-- Internal function for evaluating replay applications and generating traces.
interp :: Replay q r a -> Trace r -> Trace r -> IO (T q r a)
interp (IO a) [] ot               = do
  a' <- a
  return $ T (Right a', [], ot ++ [Result $ show a'])
interp (IO a) (Result i : it) ot  = return $ T (Right $ read i, it, ot ++ [Result i])
interp (Ask q) [] ot              = return $ T (Left q, [], ot)
interp (Ask q) (Answer i : it) ot = return $ T (Right i, it, ot ++ [Answer i])
interp (Return a) it ot           = return $ T (Right a, it, ot)
interp (Bind r f) it ot           = do
  r' <- interp r it ot
  case r' of
    T (Right a, it', ot') -> interp (f a) it' ot'
    T (Left q, it', ot')  -> return $ T (Left q, it', ot')
interp _ _ _ = error "No match in pattern!"
