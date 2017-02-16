{-# LANGUAGE GADTs, FlexibleInstances #-}
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
newtype T q r a = T (Either q a, Trace r)

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
run :: Replay q r a -> Trace r -> IO (Either (q, Trace r) a)
run r t = do
  r' <- interp r t
  case r' of
    T (Right a, t) -> return (Right a)
    T (Left q, t)  -> return (Left (q,t))

-- Internal function for evaluating replay applications and generating traces.
interp :: Replay q r a -> Trace r -> IO (T q r a)
interp (IO a) []               = do
  a' <- a
  return $ T (Right a', [Result $ show a'])
interp (IO a) (Result s : ts)  = return $ T (Right $ read s, [Result s])
interp (Ask q) []              = return $ T (Left q, [])
interp (Ask q) (Answer r : ts) = return $ T (Right r, [Answer r])
interp (Return a) _            = return $ T (Right a, [])
interp (Bind r f) ts           = do
  r' <- interp r ts
  case r' of
    T (Right a, ts')  -> do
      T (r, ts'') <- interp (f a) (tail' ts)
      return $ T (r, ts' ++ ts'')
    T (Left q, ts') -> return $ T (Left q, ts)
interp _ _ = error "No match in pattern!"

-- | Wrapper function for tail that handles empty lists as well.
tail' :: [a] -> [a]
tail' []      = []
tail' as      = tail as
