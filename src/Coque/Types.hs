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


type f $ x = f x; infixr 0 $

type Str   = String
type Op    = StackM ()
type Dict  = Map Str MOp
type Stack = LZ Object

data Env = Env
  { time  :: Time
  , dict  :: Dict
  }

data Time = Bw | Fw


type MOp      = StackMay ()
type StackM   = WriterT Str $ S.StateT Env $ TardisT Stack Stack Identity
type StackMay = MaybeT StackM

-- runStackM :: Dict -> (Stack, Stack) -> StackMay a -> (Maybe a, String)
runStackM d (l, r)
  = runIdentity
  . flip evalTardisT (lz l, lz r)
  . flip S.evalStateT Env { time = Fw, dict = d }
  . runWriterT
  . runMaybeT

data Object
  = OStr Str
  | OStack [Object]
  | OOp MOp

instance Show Object where
  show (OStr  str) = str
  show (OOp     _) = "OP"


instance (MonadTardis bw fw m) => MonadTardis bw fw (MaybeT m) where
  tardis = lift . tardis

instance (MonadTardis bw fw m) => MonadTardis bw fw (S.StateT s m) where
  tardis = lift . tardis

instance (MonadTardis bw fw m, Monoid w) => MonadTardis bw fw (WriterT w m) where
  tardis = lift . tardis


rev = \case Bw -> Fw; Fw -> Bw

getDict
  = S.gets dict

dword x = do
  Just a <- M.lookup x <$> getDict
  pure a

runDict
  = join . dword


stepQueue = deq >>= eval

eval = join . eval'

eval' = \case
  OStr   x -> dword x <|> pure do push (OStr x)
  OOp    m -> pure m

antiDo m = do
  t <- getTime
  setTime (rev t)
  x <- m
  setTime t
  pure x

output = tell . (++"\n") . show

-------------

nop = pure ()


get      = getTime >>= \case Bw -> getFuture;         Fw -> getPast
gets   a = getTime >>= \case Bw -> getsFuture a;      Fw -> getsPast a
put    a = getTime >>= \case Bw -> sendPast a;        Fw -> sendFuture a
modify a = getTime >>= \case Bw -> modifyBackwards a; Fw -> modifyForwards a

withDict  f = S.modify \s -> s { dict = f $ dict s }
withTime  f = S.modify \s -> s { time = f $ time s }

setTime = withTime . const
getTime = S.gets time


que  = modify . (:-)
push = modify . (:<)


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


left  = modify moveL
right = modify moveR

