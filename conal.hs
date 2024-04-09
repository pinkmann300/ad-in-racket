{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
-- This particular language extension has been enabled in Haskell to ensure that a class can take multiple type parameters.
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

import Distribution.Types.LocalBuildInfo (LocalBuildInfo (compiler))

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
  identity = \a -> a
  composition :: (b -> c) -> (a -> b) -> a -> c
  composition g f = \a -> g (f a)

-- The above defines functions as an instance of the category. The identity of a value passed to a function is
-- the function itself and the composition of two functions g and f will be g(f a) for some arbitrary a.

class (Category k) => Monoidal k where
  cross :: (a `k` c) -> (b `k` d) -> ((a, b) `k` (c, d))

-- Cross function takes two arguments relating two types a and c, and returns a tuple connected relation.
instance Monoidal (->) where
  cross :: (a -> c) -> (b -> d) -> (a, b) -> (c, d)
  cross f g = \(a, b) -> (f a, g b)

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
  dup = \a -> (a, a)

exln :: Integer -> [a] -> a
exln _ [] = error "Out of bounds"
exln 1 (x : xs) = x
exln n (x : xs) = exln (n - 1) xs

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
listAdd (x : xs) (y : ys) = [x + y] ++ (listAdd xs ys)
listAdd [] [] = []

negateL :: (Num a) => [a] -> [a]
negateL (xs) = map negate xs

scalarMul :: (Num a) => a -> [a] -> [a]
scalarMul k = map (* k)

dotProd :: (Num a) => [a] -> [a] -> [a]
dotProd [] [] = []
dotProd xs [] = []
dotProd [] xs = []
dotProd (x : xs) (y : ys) = [x * y] ++ dotProd xs ys

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
  identity = D (\a -> (identity a, identity))
  composition (D g) (D f) = D (\a -> let (b, fdash) = f a; (c, gdash) = g b in (c, composition gdash fdash))

-- Handles parallel composition above.
instance Monoidal D where
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
  negateC = linearD negateC
  addC = linearD addC
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

-- magSqr :: (Num t) => (t, t) -> t -- R^2 -> R function
-- magSqr = composition addC (tri (composition mulC (tri exl exl)) (composition mulC (tri exr exr)))

magSqr2 :: D (Double, Double) Double
magSqr2 = composition addC (tri (composition mulC (tri exl exl)) (composition mulC (tri exr exr)))

data Tuple a = Cons a (Tuple a) | Empty
  deriving (Show, Read)

tupLength :: Tuple a -> Integer
tupLength (Cons k m) = 1 + tupLength m
tupLength Empty = 0

-- Examples:
-- h = (stripD magSqr2) (2.0,3.0)
-- fst h  = 13.0
-- (snd h) (1.0, 0.0) -> Partial derivative w.r.t a
-- (snd h) (0.0, 1.0) -> Partial derivative w.r.t b

cosSinProd :: (Floating t) => (t, t) -> t
cosSinProd = composition mulC (composition (tri cosC sinC) mulC)

inlF :: (Num a) => a -> (a, a)
inlF a = (a, 0)

inrF :: (Num b) => b -> (b, b)
inrF b = (0, b)

jamF :: (Num a) => (a, a) -> a
jamF = uncurry (+)

k :: Integer -> NumberList
k n = fromInteger n

sqrList :: D NumberList NumberList
sqrList = composition mulC (tri identity identity)

magSqr3 :: D [Double] Double
magSqr3 = composition mulC (tri (composition mulC (tri (exlD 1) (exlD 2))) (exlD 3))

exlD :: Integer -> D [b] b
exlD n = linearD (exln n)

sqr :: (Num t) => t -> t
sqr = composition mulC (tri identity identity)

mulAb9 :: D (Double, Double) Double
mulAb9 = composition mulC (tri exl exr)

magSqr :: D [Double] Double
magSqr = composition addC (tri (composition mulC (tri (exlD 1) (exlD 1))) (composition mulC (tri (exlD 2) (exlD 2))))

mulAb10 :: D [Double] Double
mulAb10 = composition mulC (tri (exlD 1) (exlD 2))

-- Convinced in terms of correctness for addition cases.

-- Why are we limited to only the first derivative?
--  Our entire approach has been changed when we interpret derivatives as linear maps.
--  The critical theorem which we use as for saying that the derivative of a linear map
--  is the linear map itself. Hence, if we try to compute the second derivative as

-- Doubt case : magSqr3 = x * y * z
--              k = (stripD magSqr3) [2.0,3.0,4.0] -- Works correctly
--              fst k = 24.0 - Function value
--              (snd k) [1.0,0.0,0.0] = 12.0
--              (snd k) [0.0,1.0,0.0] = 8.0
--              (snd k) [0.0,0.0,1.0] = 4.0

-- However, 2 element functions with Tuple structure and the (Double, Double) functions yield the same result in terms
-- of numerical value. Hence, wondering if this constructing is correct or not.

-- Want to verify the correctness of the specification with respect to

-- Amending existing Tuple structure to resemble lists since they both have the same definition.
join k = composition addC (uncurry tri k)

unjoin :: (Cocartesian k) => k (a, b) c -> (k a c, k b c)
unjoin h = (composition h inl, composition h inr)

-- The RAD implementation begins from here.
