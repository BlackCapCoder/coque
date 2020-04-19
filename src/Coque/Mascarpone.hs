module Coque.Mascarpone where

import Coque.Types
import Coque.Interpreter


dictMascarpone = newI
  [ "reify"      --> reify
  , "deify"      --> deify
  , "extract"    --> extract
  , "install"    --> install
  , "get_parent" --> getParent
  , "set_parent" --> setParent
  , "create"     --> create
  , "expand"     --> expand
  , "perform"    --> perform
  , "null"       --> pushNull
  , "uniform"    --> pushUniform
  ]


-- Pushes the current interpreter onto the stack
reify = do
  d <- getDict
  push $ ODict d

-- Pops an interpreter from the stack and installs it as the current interpreter
deify = do
  ODict d <- pop
  setDict d

-- Pops a symbol from the stack, then pops an interpreter. It pushes onto the
-- stack the operation associated with that symbol in that interpreter.
extract = do
  OStr  sym <- pop
  ODict d   <- pop
  let op = lookupI sym d
  push $ OOp op

-- pops a symbol from the stack, then an operation, then an interpreter. It
-- pushes onto the stack a new interpreter which is the same as the given
-- interpreter, except that in it, the given symbol is associated with the
-- given operation.
install = do
  OStr  sym <- pop
  OOp   op  <- pop
  ODict d   <- pop
  let d' = insertI sym op d
  push $ ODict d'

-- pops an interpreter from the stack and pushes it's parent interpreter onto the stack.
getParent = do
  ODict d <- pop
  push $ ODict $ getParentI d

-- pops an interpreter i from the stack, then pops an interpreter j. It pushes
-- a new interpreter which is the same as i, except that it's parent interpreter is j.
setParent = do
  ODict i <- pop
  ODict j <- pop
  push . ODict $ setParentI j i

-- pops an interpreter from the stack, then a string. It creates a new
-- operation defined by how that interpreter would interpret that string of
-- symbols, and pushes that operation onto the stack.
create = do
  error "create not implemented"

expand = do
  error "expand not implemented"

-- Pops an operation from the stack and executes it.
perform = do
  OOp op <- pop
  op

-- Pushes the null interpreter onto the stack.
pushNull = do
  push $ ODict nullI

-- Pops an operation from the stack and pushes back an interpreter where all
-- symbols are associated with that operation.
pushUniform = do
  OOp op <- pop
  push . ODict $ uniformI op

