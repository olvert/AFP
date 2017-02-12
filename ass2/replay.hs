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


run :: Replay q r a -> Trace r -> IO (Either (q, Trace r) a) -- typechecks but does not do what we want..
run (IO a) []             = do  a' <- a
                                return (Right a')
run (IO a) (Result t:ts)  = return (Right $ read t)
run (Ask q) t             = return (Left(q, t))
run (Return a) t          = return (Right a)
run (Bind r f) t          = do  r' <- run r t
                                case r' of
                                  (Right a)     -> run (f a) t
                                  (Left (q,t))  -> return $ Left (q,t)

run _ _                   = error "No match in pattern"
