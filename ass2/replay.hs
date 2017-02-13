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

addResult  :: Trace r -> String -> Trace r
addResult t s = Result s : t

-- TESTING

newtype T q r a = T (Either q a, Trace r)

interp :: Replay q r a -> Trace r -> IO (T q r a)
interp (IO a) []               = do a' <- a; return $ T (Right a', [Result $ show a'])
interp (IO a) (Result s : ts)  = return $ T (Right $ read s, [Result s])
interp (Ask q) []      = return $ T (Left q, [])
interp (Ask q) (Answer r : ts)      = return $ T (Right r, [Answer r])
interp (Return a) _   = return $ T (Right a, [])
interp (Bind r f) ts   = do   r' <- interp r ts
                              case r' of
                                T (Right a, ts')  -> do T (t, ts'') <- interp (f a) (tail' ts)
                                                        return $ T (t, ts' ++ ts'')
                                T (Left q, ts') -> return $ T (Left q, ts)

interp _ _ = error "No match in pattern!"

tail' :: [a] -> [a]
tail' []      = []
tail' as      = tail as

run :: Replay q r a -> Trace r -> IO (Either (q, Trace r) a)
run r t = do  r' <- interp r t
              case r' of
                T (Right a, t) -> return (Right a)
                T (Left q, t) -> return (Left (q,t))
