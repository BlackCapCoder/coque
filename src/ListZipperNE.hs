-- non-empty version
module ListZipperNE where

import Stack
import Control.Comonad


data LZ a = LZ [a] a [a]
  deriving (Functor, Foldable, Traversable)


modifyL f (LZ l x r) = LZ (f l) x r
modifyR f (LZ l x r) = LZ l x (f r)
modify  f (LZ l x r) = LZ l (f x) r

replace
  = modify . const

pushL = modifyL . push
pushR = modifyR . push


instance Comonad LZ
  where
    extract
      (LZ _ x _) = x

    duplicate
      = move moveL moveR


moveL = \case
  LZ (l :- ls) x rs -> LZ ls l (x :- rs)
  lz                -> lz

moveR = \case
  LZ ls x (r :- rs) -> LZ (x :- ls) r rs
  lz                -> lz

move a b z = LZ (f a z) z (f b z)
  where
    f g = tail . iterate g

