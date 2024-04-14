{-# HLINT ignore "Use tuple-section" #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

import Control.Arrow qualified as Control

-- Add more instances for other types as needed

class Category k where
  identity :: a `k` a
  composition :: (b `k` l) -> (a `k` b) -> (a `k` l)

instance Category (->) where
  identity :: a -> a
  identity = id
  composition :: (b -> c) -> (a -> b) -> a -> c
  composition g f = g . f

class (Category k) => Monoidal k where
  cross :: forall a b d c. (a `k` c) -> (b `k` d) -> ((a, b) `k` (c, d))

class (Monoidal k) => Cartesian k where
  exl :: (a, b) `k` a
  exr :: (a, b) `k` b
  dup :: a `k` (a, a)

instance Monoidal (->) where
  cross :: (a -> k) -> (b -> d) -> ((a, b) -> (k, d))
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

newtype D a b = D (forall c1. a -> (b, (b -> c1) -> a -> c1))

instance Category D where
  identity :: D a a
  identity = linearD id
  composition :: D b l -> D a b -> D a l
  composition (D g) (D f) = D (\a -> let (b, f') = f a; (c, g') = g b in (c, f' . g'))

instance Monoidal D where
  cross :: D a c -> D b d -> D (a, b) (c, d)
  cross (D f) (D g) = D (\(a, b) -> let (fa, fa') = f a; (gb, gb') = g b in ((fa, gb), (. cross (fa' id) (gb' id))))

instance Cartesian D where
  exl = linearD exl
  exr = linearD exr
  dup = linearD dup

tri :: (Cartesian k) => k b c -> k b d -> k b (c, d)
tri f g = composition (cross f g) dup

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

instance (Floating a) => FloatCat (->) a where
  sinC :: (Floating a) => a -> a
  sinC = sin
  cosC :: (Floating a) => a -> a
  cosC = cos
  asinC :: (Floating a) => a -> a
  asinC = asin
  acosC :: (Floating a) => a -> a
  acosC = acos
  atanC :: (Floating a) => a -> a
  atanC = atan
  sinhC :: (Floating a) => a -> a
  sinhC = sinh
  coshC :: (Floating a) => a -> a
  coshC = cosh
  asinhC :: (Floating a) => a -> a
  asinhC = asinh
  acoshC :: (Floating a) => a -> a
  acoshC = acosh
  atanhC :: (Floating a) => a -> a
  atanhC = atanh
  logC :: (Floating a) => a -> a
  logC = log
  expC :: (Floating a) => a -> a
  expC = exp

class Scalable k a where
  scaleNum :: a -> (a `k` a)

instance (Num a) => Scalable (->) a where
  scaleNum = (*)

class (Category k) => Cocartesian k where
  inl :: a `k` (a, b)
  inr :: b `k` (a, b)
  jam :: (a, a) `k` a

class NumCat k a where
  negateC :: a `k` a
  addC :: (a, a) `k` a
  mulC :: (a, a) `k` a

instance (Num a) => NumCat (->) a where
  negateC :: (Num a) => a -> a
  negateC = negate
  addC :: (Num a) => (a, a) -> a
  addC = uncurry (+)
  mulC :: (Num a) => (a, a) -> a
  mulC = uncurry (*)

instance (Num a) => NumCat D a where
  negateC :: (Num a) => D a a
  negateC = linearD negateC
  addC :: (Num a) => D (a, a) a
  addC = linearD addC
  mulC :: (Num a) => D (a, a) a
  mulC = D (\(a, b) -> (a * b, (. composition (uncurry (+)) (cross (scaleNum b) (scaleNum a)))))

stripD :: D a b -> a -> (b, (b -> c1) -> a -> c1)
stripD (D f) = f 

exln :: Integer -> [a] -> a
exln _ [] = error "Out of bounds"
exln 1 (x : xs) = x
exln n (x : xs) = exln (n - 1) xs

exlD :: Integer -> D [b] b
exlD n = linearD (exln n) 


-- Test functions 

-- R -> R case : Checked and works 
sqr :: D Double Double
sqr = composition mulC (tri identity identity)

-- R^n -> R case : 
xyzProd :: D [Integer] Integer
xyzProd = composition mulC (tri (exlD 3) (composition mulC (tri (exlD 1) (exlD 2)))) 
