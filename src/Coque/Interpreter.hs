module Coque.Interpreter where

import Data.Map.Lazy as M


data Interpreter sym op
  = Interp (Map sym op) (sym -> op) (Interpreter sym op)

instance Ord sym => Semigroup (Interpreter sym op) where
  Interp ma da pa <> Interp mb db pb
    = Interp (ma <> mb) db pb

instance (Ord sym, Applicative op) => Monoid (Interpreter sym (op ())) where
  mempty = emptyI


emptyI
  = Interp mempty (\_ -> pure ()) emptyI

uniformI op
  = Interp mempty (const op) emptyI

nullI
  = uniformI $ error "Attempted to use null interpreter"

newI xs
  = Interp (M.fromList xs) (\_ -> pure ()) emptyI

lookupI sym (Interp m def _)
  = case M.lookup sym m of
      Just op -> op
      Nothing -> def sym

insertI sym op (Interp m d p)
  = Interp (M.insert sym op m) d p

getParentI (Interp _ _ p)
  = p

setParentI p (Interp m d _)
  = Interp m d p

getDefI (Interp _ d _)
  = d

setDefI d (Interp m _ p)
  = Interp m d p

