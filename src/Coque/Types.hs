{-# LANGUAGE UndecidableInstances #-}
module Coque.Types where

import Text.Trifecta
import Data.Map.Lazy as M
import Control.Monad.Fix
import Control.Monad.Trans.Maybe
import Control.Monad.Tardis
import Control.Monad.Writer.Lazy
import Control.Monad.State qualified as S
import Data.Functor.Identity
import Data.Foldable
import Stack qualified as S
import Stack (pattern (:-))
import ListZipper
import Coque.Interpreter


type f $ x = f x; infixr 0 $

type Str   = String
type Op    = StackM ()
type Dict  = Interpreter Str Op
type Stack = LZ Object

type StackM
  = MaybeT $ WriterT Str $ S.StateT Env $ TardisT Stack Stack Identity


data Time
  = Bw | Fw

data Env = Env
  { time  :: Time
  , dict  :: Dict
  }


evalStackM' d (l, r)
  = evalStackM d (lz l, lz r)

evalStackM d st
  = runIdentity
  . flip evalTardisT st
  . flip S.evalStateT Env { time = Fw, dict = d }
  . runWriterT
  . runMaybeT

runStackM d st
  = runIdentity
  . flip runTardisT st
  . flip S.runStateT Env { time = Fw, dict = d }
  . runWriterT
  . runMaybeT


data Object
  = OStr  Str
  | OOp   Op
  | ODict Dict


instance Show Object where
  show (OStr  str) = str
  show (OOp     _) = "<OP>"
  show (ODict   _) = "<DICT>"


instance (MonadTardis bw fw m) => MonadTardis bw fw (MaybeT m) where
  tardis = lift . tardis

instance (MonadTardis bw fw m) => MonadTardis bw fw (S.StateT s m) where
  tardis = lift . tardis

instance (MonadTardis bw fw m, Monoid w) => MonadTardis bw fw (WriterT w m) where
  tardis = lift . tardis


output
  = tell . (++"\n") . show

dword x
  = lookupI x <$> getDict

eval
  = join . eval'

eval' = \case
  OStr   x -> dword x <|> pure do push (OStr x)
  OOp    m -> pure m

evalLoop
  = forever do deq >>= eval

antiDo m = do
  t <- getTime
  setTime (rev t)
  x <- m
  setTime t
  pure x

fork = do
  s <- get
  d <- getDict
  let (((r, str), env), (bw, fw)) = runStackM d (lz [], s) evalLoop
  withDict $ const $ dict env
  tell str
  put bw


-------------


rev = \case
  Bw -> Fw
  Fw -> Bw

nop   = pure ()
(-->) = (,); infixr 0 -->

left  = modify moveL
right = modify moveR

get      = getTime >>= \case Bw -> getFuture;         Fw -> getPast
gets   a = getTime >>= \case Bw -> getsFuture a;      Fw -> getsPast a
put    a = getTime >>= \case Bw -> sendPast a;        Fw -> sendFuture a
modify a = getTime >>= \case Bw -> modifyBackwards a; Fw -> modifyForwards a

withDict  f = S.modify \s -> s { dict = f $ dict s }
withTime  f = S.modify \s -> s { time = f $ time s }

getTime = S.gets time
setTime = withTime . const

getDict = S.gets dict
setDict = withDict . const


que  = modify . (:-)
push = modify . (:<)

tradeQueues = do
  fw <- getPast
  bw <- getFuture
  sendPast   fw
  sendFuture bw


pop = do
  t <- getTime
  case t of
    Fw -> do
      x :< xs <- get
      x <$ put xs
    Bw -> do
      rec
        put xs
        ~(x :< xs) <- get
      pure x

deq = do
  t <- getTime
  case t of
    Fw -> do
      x :- xs <- get
      x <$ put xs
    Bw -> do
      rec
        put xs
        ~(x :- xs) <- get
      pure x

