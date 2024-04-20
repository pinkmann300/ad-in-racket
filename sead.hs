-- An attempt at R^m -> R^n functions

import Data.Ix
import GHC.Arr

class Category k where
  identity :: a `k` a
  composition :: (b `k` c) -> (a `k` b) -> (a `k` c)

class (Category k) => Monoidal k where
  cross :: (a `k` c) -> (b `k` d) -> ((a, b) `k` (c, d))

class NumCat k a where
  negateC :: a `k` a
  addC :: (a, a) `k` a
  mulC :: (a, a) `k` a

class (Monoidal k) => Cartesian k where
  exl :: (a, b) `k` a
  exr :: (a, b) `k` b
  dup :: a `k` (a, a)

newtype D a b = D (a -> (b, a -> b))

instance Category (->) where
  identity = id
  composition :: (b -> c) -> (a -> b) -> a -> c
  composition = (.)

instance Monoidal (->) where
  cross :: (a -> c) -> (b -> d) -> (a, b) -> (c, d)
  cross f g (a, b) = (f a, g b)

instance Cartesian (->) where
  exl :: (a, b) -> a
  exl = fst
  exr :: (a, b) -> b
  exr = snd
  dup :: a -> (a, a)
  dup a = (a, a)

instance Category D where
  identity :: D a a
  identity = D (\a -> (identity a, identity))
  composition :: D b c -> D a b -> D a c
  composition (D g) (D f) = D (\a -> let (b, fdash) = f a; (c, gdash) = g b in (c, composition gdash fdash))

newFun :: (Num a, Ix i) => D (Array i a) (Array i a)
newFun = identity

stripD :: D a b -> (a -> (b, a -> b))
stripD (D f) = f

-- a :: (Num n, Num i) => Array i n 
-- a = (array (1, 4) [(1, 2), (2, 3), (3, 4), (4, 5)])

arrayAdd :: (Num a, Ix i) => Array i a -> Array i a -> Array i a
arrayAdd a b = if bounds a == bounds b then array (bounds a) [(i, a!i + b!i) | i <- range (bounds a)] else error "Not compatible for addition"

scalarMultiply :: (Num a, Ix i) => a -> Array i a -> Array i a
scalarMultiply scalar = fmap (* scalar)

arrayNegate :: (Num a, Ix i) =>  Array i a -> Array i a
arrayNegate = scalarMultiply (-1)

instance (Num a, Ix i, Num i) => Num (Array i a) where
  (+) = arrayAdd
  (*) = arrayAdd
  abs = fmap abs
  signum = fmap signum
  fromInteger n = array (1, 1) [(1,fromInteger n)]
  negate = scalarMultiply (-1)

-- Num instance of arrays covered. 
