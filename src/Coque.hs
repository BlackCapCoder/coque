module Coque where

import Text.Trifecta
import Data.Map qualified as M
import Data.Maybe
import Data.Foldable
import Coque.Parser
import Coque.Types
import Coque.Interpreter
import Coque.Mascarpone


execFile dict pth = do
  r <- parseFromFile parseCode pth
  case r of
    Nothing -> pure ()
    Just ws -> putStr . snd $ evalStackM' dict ([], ws) evalLoop

-------------

pushUnknown
  = setDefI \sym -> push $ OStr sym

dictAll = pushUnknown $ mconcat
  [ mempty
  , dictDebug
  , dictAnti
  , dictStack
  , dictMascarpone
  ]


dictDebug = newI
  [ "print" --> pop >>= output
  ]

dictAnti = newI
  [ "que"  --> antiDo . eval =<< pop
  , "push" --> antiDo . push =<< pop
  , "fork" --> fork

  , "pop"  --> antiDo pop >>= push
  , "anti" --> anti
  ]

dictStack = newI
  [ "<"    --> left
  , ">"    --> right
  , "dup"  --> dup
  , "swap" --> swap
  ]


------------


dup = do
  x <- pop
  push x
  push x

swap = do
  x <- pop
  y <- pop
  push x
  push y

anti =
  pop >>= eval' >>= antiDo

revTime = do
  t <- getTime
  setTime (rev t)

