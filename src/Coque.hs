module Coque where

import Text.Trifecta
import Data.Map qualified as M
import Data.Maybe
import Data.Foldable
import Coque.Parser
import Coque.Types
import Control.Monad.Trans.Maybe
import Control.Monad.Trans
import Control.Monad.Tardis
import Control.Monad.Fix
import Debug.Trace


execFile dict pth = do
  r <- parseFromFile parseCode pth
  case r of
    Nothing -> pure ()
    Just ws -> putStr . snd $ runStackM dict ([], ws) do
      forever
        stepQueue

-------------

smallDict = M.fromList
  [ "print" --> pop >>= output

  , "def"   --> def
  , "undef" --> undef

  , "<"  --> left
  , ">"  --> right
  , "que"  --> antiDo . eval =<< pop
  , "push" --> antiDo . push =<< pop


  , "dup"   --> dup
  , "swap"  --> swap

  , "anti"  --> anti
  , "halt"  --> mzero
  ]
  where (-->) = (,); infixr 0 -->


anti = pop >>= eval' >>= antiDo

revTime = do
  t <- getTime
  setTime (rev t)

dup = do
  x <- pop
  push x
  push x

swap = do
  x <- pop
  y <- pop
  push x
  push y

def = do
  OStr name <- pop
  obj       <- pop
  withDict . M.insert name =<< eval' obj

undef = do
  OStr name <- pop
  withDict $ M.delete name

