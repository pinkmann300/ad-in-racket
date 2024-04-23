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

instance Monoidal D where
  cross :: D a c -> D b d -> D (a, b) (c, d)
  cross (D f) (D g) = D (\(a, b) -> let (c, fdash) = f a; (d, gdash) = g b in ((c, d), cross fdash gdash))

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
arrayAdd a b = if bounds a == bounds b then array (bounds a) [(i, a ! i + b ! i) | i <- range (bounds a)] else error "Not compatible for addition"

scalarMultiply :: (Num a, Ix i) => a -> Array i a -> Array i a
scalarMultiply scalar = fmap (* scalar)

arrayNegate :: (Num a, Ix i) => Array i a -> Array i a
arrayNegate = scalarMultiply (-1)

tri :: (Cartesian k) => k b c -> k b d -> k b (c, d)
tri f g = composition (cross f g) dup

instance (Num a, Ix i, Num i) => Num (Array i a) where
  (+) = arrayAdd
  (*) = arrayAdd
  abs = fmap abs
  signum :: (Num a, Ix i, Num i) => Array i a -> Array i a
  signum = fmap signum
  fromInteger n = array (1, 1) [(1, fromInteger n)]
  negate = scalarMultiply (-1)

-- Num instance of arrays covered.

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
  mulC = D (\(a, b) -> (a * b, composition addC (cross (scaleNum b) (scaleNum a))))

scaleNum :: Num a => a -> a -> a
scaleNum = (*)

linearD :: (a -> b) -> D a b
linearD f = D (\a -> (f a, f))

instance Cartesian D where
  exl :: D (a, b) a
  exl = linearD exl
  exr :: D (a, b) b
  exr = linearD exr
  dup :: D a (a, a)
  dup = linearD dup

firstFun :: (Ix i, Num i) => D (Array i Integer) (Array i Integer )
firstFun = composition addC (tri identity identity)

crossn :: (Ix i, Num i, Enum i) => i -> Array i (t -> e) -> t -> Array i e
crossn n arr val = array (1, n) [(i, (arr ! i) val)| i <- [1..n]]


