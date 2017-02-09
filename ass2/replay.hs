{-# LANGUAGE GADTs, FlexibleInstances #-}
module Replay (
  Replay, Trace,

  io, ask, 

  emptyTrace, addAnswer,

  run    
  ) where

import Control.Applicative
import Control.Monad (liftM, ap)

-- Types
data Replay q r a where
  IO :: (Show a, Read a) => IO a -> Replay q r a
  Ask :: q -> Replay q r r
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
io = IO
ask :: q -> Replay q r r
ask = Ask

emptyTrace :: Trace r
emptyTrace = []
addAnswer  :: Trace r -> r -> Trace r
addAnswer t a = t ++ [Answer a]

run :: Replay q r a -> Trace r -> IO (Either (q, Trace r) a)
-- run IO a b t = do
--   show t
--   return Left ((), t)
run = undefined