module Coque where

import Text.Trifecta
import Data.Map qualified as M
import Data.Maybe
import Data.Foldable
import Coque.Parser
import Coque.Types


execFile dict pth = do
  r <- parseFromFile parseCode pth
  case r of
    Nothing -> pure ()
    Just ws -> putStr . snd $ evalStackM' dict ([], ws) evalLoop

-------------

(-->) = (,); infixr 0 -->


dictAll = mconcat
  [ mempty
  , dictDebug
  , dictDef
  , dictAnti
  , dictStack
  , dictMics
  ]


dictDebug = M.fromList
  [ "print" --> pop >>= output
  , "halt"  --> mzero
  ]

dictDef = M.fromList
  [ "def"   --> def
  , "undef" --> undef
  ]

dictAnti = M.fromList
  [ "que"  --> antiDo . eval =<< pop
  , "push" --> antiDo . push =<< pop
  , "fork" --> fork

  , "pop"  --> antiDo pop >>= push
  , "anti" --> anti
  ]

dictStack = M.fromList
  [ "<"    --> left
  , ">"    --> right
  , "dup"  --> dup
  , "swap" --> swap
  ]

dictMics = M.fromList
  [
  ]


------------


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

