module Queue where

import Stack


data Queue a = Queue [a] [a]
  deriving (Foldable, Traversable, Functor, Show)

data QS a = QS { stack :: [a], queue :: Queue a }
  deriving (Foldable, Traversable, Functor, Show)


qs xs = QS [] (Queue xs [])

cheat x (QS l (Queue ql qr)) = QS l (Queue (push x ql) qr)

que x (QS l r) = QS l (push x r)


instance IsStack Queue where
  push x (Queue l r) = Queue l (x:r)

  pop (Queue (x:xs) y) = Just (x, Queue xs y)
  pop (Queue _     []) = Nothing
  pop (Queue _      y) = pop (Queue (reverse y) [])

instance IsStack QS where
  push x (QS l r) = QS (push x l) r
  pop    (QS l r) = fmap (QS l) <$> pop r

-----

instance Semigroup (Queue a) where
  Queue a b <> Queue c d = Queue (a <> reverse b <> c) d

instance Monoid (Queue a) where
  mempty = Queue mempty mempty


instance Semigroup (QS a) where
  QS a b <> QS c d = QS (a <> c) (b <> d)

instance Monoid (QS a) where
  mempty = QS mempty mempty

