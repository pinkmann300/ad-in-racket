{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
-- This particular language extension has been enabled in Haskell to ensure that a class can take multiple type parameters.
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use camelCase" #-}

import Data.Ix
import GHC.Arr

-- The purpose of the current state is to handle functions of the type R -> R (Real to Real) functions. We will set it up using the category based vocabulary.
-- We define the vocabulary required for writing functions as a category type's instance. Hence, we begin with defining the categories.
-- The type parameter k mentioned below will always take 2 parameters for its own type definition. A domain and a codomain type.

-- (a `x` b) definition is just another synonym for tuples

type NumberList = [Double]

class Category k where
  identity :: a `k` a
  composition :: (b `k` c) -> (a `k` b) -> (a `k` c)

-- The above defines a class named Categories which takes a parameter k. Each category instance will have an identity function which is of the type a to a.
-- Each category will also have a composition function where it takes two morphisms and composes the 2 functions together.

instance Category (->) where
  identity :: a -> a
  composition :: (b -> c) -> (a -> b) -> a -> c
  identity a = a
  composition g f a = g (f a)

-- The above defines functions as an instance of the category. The identity of a value passed to a function is
-- the function itself and the composition of two functions g and f will be g(f a) for some arbitrary a.

class (Category k) => Monoidal k where
  cross :: (a `k` c) -> (b `k` d) -> ((a, b) `k` (c, d))

-- Cross function takes two arguments relating two types a and c, and returns a tuple connected relation.
instance Monoidal (->) where
  cross :: (a -> c) -> (b -> d) -> (a, b) -> (c, d)
  cross f g (a, b) = (f a, g b)

class (Monoidal k) => Cartesian k where
  exl :: (a, b) `k` a
  exr :: (a, b) `k` b
  dup :: a `k` (a, a)

instance Cartesian (->) where
  exl :: (a, b) -> a
  exl = fst
  exr :: (a, b) -> b
  exr = snd
  dup :: a -> (a, a)
  dup a = (a, a)

exln :: Integer -> [a] -> a
exln _ [] = error "Out of bounds"
exln 1 (x : xs) = x
exln n (x : xs) = exln (n - 1) xs

leftin :: (Num a, Ix i) => i -> Array i a -> a
leftin n xs = xs ! n

-- Goal :: Construct some functions using the category vocabulary.

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

instance Num NumberList where
  (+) :: NumberList -> NumberList -> NumberList
  (+) = zipWith (+)
  (*) = zipWith (*)
  negate = map negate
  abs = map abs
  signum = map signum
  fromInteger n = replicate 1000 (fromInteger n)

-- The uncurry function essentially allows us to

listAdd :: (Num a) => [a] -> [a] -> [a]
listAdd (x : xs) (y : ys) = (x + y) : listAdd xs ys
listAdd [] [] = []

negateL :: (Num a) => [a] -> [a]
negateL xs = map negate xs

scalarMul :: (Num a) => a -> [a] -> [a]
scalarMul k = map (* k)

dotProd :: (Num a) => [a] -> [a] -> [a]
dotProd [] [] = []
dotProd xs [] = []
dotProd [] xs = []
dotProd (x : xs) (y : ys) = (x * y) : dotProd xs ys

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

tri :: (Cartesian k) => k b c -> k b d -> k b (c, d)
tri f g = composition (cross f g) dup

-- Until here, we can now see numbers which is a good thing.

-- Defining the newtype for differentiable functions itself
-- Making use of the fact that all linear maps are functions
-- themselves at the end of the day.

-- Since linear maps have different representations, should I just declare them as a type union of
-- all the possible forms a linear map could take ?

-- Why is D defined the way it has been defined?
-- D is a type which denotes functions but the catch is its a different type of functions we will
-- refer to as "differentiable functions". A function of type D takes an argument from the domain
-- and maps it to a tuple where the first element of the tuple stores the value of the function
-- application to the argument and the second element of the tuple contains the linear map representation
-- that we are working with.

class Scalable k a where
  scaleNum :: a -> (a `k` a)

instance (Num a) => Scalable (->) a where
  scaleNum = (*)

class (Category k) => Cocartesian k where
  inl :: a `k` (a, b)
  inr :: b `k` (a, b)
  jam :: (a, a) `k` a

linearD :: (a -> b) -> D a b
linearD f = D (\a -> (f a, f))

newtype D a b = D (a -> (b, a -> b))

-- Handles sequential composition above for differentiable functions
instance Category D where
  identity :: D a a
  identity = D (\a -> (identity a, identity))
  composition :: D b c -> D a b -> D a c
  composition (D g) (D f) = D (\a -> let (b, fdash) = f a; (c, gdash) = g b in (c, composition gdash fdash))

-- Handles parallel composition above.
instance Monoidal D where
  cross :: D a c -> D b d -> D (a, b) (c, d)
  cross (D f) (D g) = D (\(a, b) -> let (c, fdash) = f a; (d, gdash) = g b in ((c, d), cross fdash gdash))

-- Essentially takes 2 funtions of the differentiable type and returns another differentiable function
instance Cartesian D where
  exl :: D (a, b) a
  exl = linearD exl
  exr :: D (a, b) b
  exr = linearD exr
  dup :: D a (a, a)
  dup = linearD dup

jamD :: (Num t) => (t, t) -> t
jamD = uncurry (+)

instance (Num a) => NumCat D a where
  negateC :: (Num a) => D a a
  negateC = linearD negateC
  addC :: (Num a) => D (a, a) a
  addC = linearD addC
  mulC :: (Num a) => D (a, a) a
  mulC = D (\(a, b) -> (a * b, composition jamD (cross (scaleNum b) (scaleNum a))))

instance (Floating a) => FloatCat D a where
  sinC = D (\k -> (sin k, scaleNum (cos k)))
  cosC = D (\k -> (cos k, scaleNum (negateC (sin k))))
  asinC = D (\k -> (asin k, scaleNum (recip (sqrt (1 - (k * k))))))
  acosC = D (\k -> (acos k, scaleNum (negateC (recip (sqrt (1 - k * k))))))
  atanC = D (\k -> (atan k, scaleNum (recip ((k * k) + 1))))
  sinhC = D (\k -> (sinh k, scaleNum (cosh k)))
  coshC = D (\k -> (cosh k, scaleNum (sinh k)))
  asinhC = D (\k -> (asinh k, scaleNum (recip (sqrt ((k * k) + 1)))))
  acoshC = D (\k -> (acosh k, scaleNum (negateC (recip (sqrt ((k * k) - 1))))))
  atanhC = D (\k -> (atanh k, scaleNum (recip (1 - (k * k)))))
  logC = D (\k -> (log k, scaleNum (recip k)))
  expC = D (\k -> (exp k, scaleNum (exp k)))

stripD :: D a b -> (a -> (b, a -> b))
stripD (D f) = f

-- Example functions
mulAb :: D (Double, Double) Double
mulAb = composition mulC (tri exl exr)

cube :: (Num t) => D t t
cube = composition mulC (tri identity (composition mulC (tri identity identity)))

sqr2 :: D Double Double
sqr2 = composition addC (tri identity identity)

sqr3 :: (Num t) => t -> t
sqr3 = composition mulC (tri identity identity)

magSqr2 :: D (Double, Double) Double
magSqr2 = composition addC (tri (composition mulC (tri exl exl)) (composition mulC (tri exr exr)))

data Tuple a = Cons a (Tuple a) | Empty
  deriving (Show, Read)

tupLength :: Tuple a -> Integer
tupLength (Cons k m) = 1 + tupLength m
tupLength Empty = 0

exlD :: Integer -> D [b] b
exlD n = linearD (exln n)

leftinD :: (Num a, Ix i) => i -> D (Array i a) a
leftinD n = linearD (leftin n)

cosSinProd :: (Floating t) => (t, t) -> t
cosSinProd = composition mulC (composition (tri cosC sinC) mulC)

inlF :: (Num a) => a -> (a, a)
inlF a = (a, 0)

inrF :: (Num b) => b -> (b, b)
inrF b = (0, b)

jamF :: (Num a) => (a, a) -> a
jamF = uncurry (+)

-- xyzProd works fine as a function and when supplied with derivatives
-- xyzProd :: D [Integer] Integer
-- xyzProd = composition mulC (tri (exlD 3) (composition mulC (tri (exlD 1) (exlD 2))))

k :: Integer -> NumberList
k n = fromInteger n

join :: (NumCat k c, Cartesian k) => (k a c, k a c) -> k a c
join k = composition addC (uncurry tri k)

unjoin :: (Cocartesian k) => k (a, b) c -> (k a c, k b c)
unjoin h = (composition h inl, composition h inr)

rmToRnD :: [D [Integer] Integer] -- Functions of the type Rn -> Rm
rmToRnD = [composition mulC (tri (exlD 2) (exlD 1)), composition mulC (tri (exlD 1) (exlD 3))]

funVal :: [a] -> [D [a] b] -> [b]
funVal k xs = map (\x -> fst (stripD x k)) xs

deriv :: [a] -> [D [a] b] -> [[a] -> b]
deriv k xs = map (\x -> snd (stripD x k)) xs

-- Define a function to generate a specific vector
generateVector :: Double -> Double -> [Double]
generateVector i n = [if j == i - 1 then 1 else 0 | j <- [0 .. n - 1]]

-- Example usage:
-- generateVector 1 3 returns [1,0,0]
-- generateVector 2 3 returns [0,1,0]
-- generateVector 3 3 returns [0,0,1]

-- ApplyFunList essentially allows you to apply the arguments provided in a list to be applied to the
-- individual components in the function

-- With this, we have a mechanism for defining functions of the type R^n -> R^m

-- Examples of R^n -> R function types

-- Define a function that takes a 3-dimensional vector as input
-- and produces a 2-dimensional vector as output

-- Example usage:
-- f [1.0, 2.0, 3.0] returns [3.0, -1.0]

a = array (1, 4) [(1, 2), (2, 3), (3, 4), (4, 5)]

xyzProd :: (Num a, Ix i, Num i) => D (Array i a) a
xyzProd = composition mulC (tri (leftinD 3) (composition mulC (tri (leftinD 1) (leftinD 2))))

