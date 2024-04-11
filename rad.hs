{-# HLINT ignore "Use tuple-section" #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

class Category k where
  identity :: a `k` a
  composition :: (b `k` c) -> (a `k` b) -> (a `k` c)

instance Category (->) where
  identity :: a -> a
  identity = id
  composition :: (b -> c) -> (a -> b) -> a -> c
  composition g f = g . f

class (Category k) => Monoidal k where
  cross :: (a `k` c) -> (b `k` d) -> ((a, b) `k` (c, d))

class (Monoidal k) => Cartesian k where
  exl :: (a, b) `k` a
  exr :: (a, b) `k` b
  dup :: a `k` (a, a)

newtype D a b = D (forall c. a -> (b, (b -> c) -> a -> c))

instance Category D where
  identity :: D a a
  identity = D (\a -> (a, (. id)))
  composition :: D b c -> D a b -> D a c
  composition (D g) (D f) = D (\a -> let (b, f0) = f a; (c, g0) = g b in (c, f0 . g0))

instance Monoidal (->) where
  cross :: (a -> c) -> (b -> d) -> ((a, b) -> (c, d))
  cross f g (a, b) = (f a, g b)

instance Cartesian (->) where
  exl :: (a, b) -> a
  exl = fst
  exr :: (a, b) -> b
  exr = snd
  dup :: a -> (a, a)
  dup a = (a, a)

linearD :: (a -> b) -> D a b
linearD f = D (\a -> (f a, (. f)))

stripD :: D a b -> (a -> (b, (b -> Double) -> a -> Double))
stripD (D f) = f


instance Monoidal D where
    cross (D f) (D g) = D (\(a,b) -> let (c, f') = f a; (d, g') = g b in ((c, d), \ dc ab' -> let ((c', e'), h') = dc (c, e) ab' in join (f', g') (c', e')))


inl :: AdditiveFunction f => a -> (a, f)
inl a = (a, zero)

inr b = (0, zero)

jam :: Num k => (k, k) -> k
jam = uncurry (+)

join (f, g) = jam . cross f g
join :: (Num k) => (a -> k, b -> k) -> (a, b) -> k

unjoin :: (Num b, Num a1) => ((a1, b) -> c) -> (a1 -> c, a2 -> c)
unjoin h = (h . inl, h . inr)

radCross :: (Num b1, Num b2) => forall c.  D a1 b2 -> D a2 b1 -> D (a1, a2) (b2, b1)
radCross (D f) (D g) = D (\(a,b) -> let (c, f') = f a; (d, g') = g b in ((c, d), join . cross f' g' . unjoin))


class AdditiveFunction f where
    zero :: f
    add :: f -> f -> f

instance Num k => AdditiveFunction k  where
    zero = 0
    add = (+)

