module ListZipper where

import Stack


data LZ a = LZ { left, right :: [a] }
  deriving (Foldable, Traversable, Functor, Show)

lz xs = LZ mempty xs

instance IsStack LZ
  where
    pop = \case
      LZ xs (y :- ys) -> Just (y, LZ xs ys)
      _               -> Nothing

    push y (LZ xs ys)
      = LZ xs (y :- ys)


popL = \case
  LZ (x :- xs) ys -> Just (x, LZ xs ys)
  _               -> Nothing

pushL x (LZ xs ys)
  = LZ (x :- xs) ys

pattern x :< xs <- (popL->Just (x, xs))
  where (:<) = pushL


moveL = \case
  LZ (x :- xs) ys -> LZ xs (x :- ys)
  LZ xs        ys -> LZ xs ys

moveR = \case
  LZ xs (y :- ys) -> LZ (y :- xs) ys
  LZ xs ys        -> LZ xs        ys

move a b z = LZ (f a z) (z :- f b z)
  where
    f g = tail . iterate g


duplicate
  = move moveL moveR

extend f
  = duplicate . fmap f

