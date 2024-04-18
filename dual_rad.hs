-- RAD for functions with a scalar domain.
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}


class Category k where
  identity :: a `k` a
  composition :: (b `k` c) -> (a `k` b) -> (a `k` c)

instance Category (->) where
  identity :: a -> a
  composition :: (b -> c) -> (a -> b) -> a -> c
  identity a = a
  composition g f a = g (f a)

class (Category k) => Monoidal k where
  cross :: (a `k` c) -> (b `k` d) -> ((a, b) `k` (c, d)) 

class FloatCat k a where
  sinC :: a `k` a
  cosC :: a `k` a
  asinC :: a `k` a
  acosC :: a `k` a
  atanC :: a `k` a
  sinhC :: a `k` a
  coshC :: a `k` a
  asinhC :: a `k` a
  acoshC :: a `k` a
  atanhC :: a `k` a
  logC :: a `k` a
  expC :: a `k` a

