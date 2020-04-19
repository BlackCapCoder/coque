module Stack where

import Data.List


class IsStack s where
  pop  :: s a -> Maybe (a, s a)
  push :: a -> s a -> s a


pattern x :- xs <- (pop->Just (x, xs))
  where x :- xs  = push x xs

pattern Nil <- (pop->Nothing)


instance IsStack [] where
  push = (:)
  pop  = uncons

