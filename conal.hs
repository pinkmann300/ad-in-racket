{-# LANGUAGE MultiParamTypeClasses #-} -- This particular language extension has been enabled in Haskell to ensure that a class can take multiple type parameters.
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use id" #-}
{-# HLINT ignore "Use bimap" #-}

-- The purpose of the current state is to handle functions of the type R -> R (Real to Real) functions. We will set it up using the category based vocabulary.
-- We define the vocabulary required for writing functions as a category type's instance. Hence, we begin with defining the categories. 
-- The type parameter k mentioned below will always take 2 parameters for its own type definition. A domain and a codomain type. 

-- (a `x` b) definition is just another synonym for tuples

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

class Category k => Monoidal k where
    cross :: (a `k` c) -> (b `k` d) -> ((a,b) `k` (c,d))

-- Cross function takes two arguments relating two types a and c, and returns a tuple connected relation.

instance Monoidal (->) where
    cross :: (a -> c) -> (b -> d) -> (a, b) -> (c, d)
    cross f g = \(a,b) -> (f a, g b)

class Monoidal k => Cartesian k where
    exl :: (a, b) `k` a
    exr :: (a, b) `k` b 
    dup :: a `k` (a, a)

instance Cartesian (->) where
    exl :: (a, b) -> a
    exl = fst
    exr :: (a, b) -> b
    exr = snd
    dup :: a -> (a, a)
    dup = \a -> (a,a)


class Category k => Cocartesian k where
    inl :: a `k` (a, b)
    inr :: b `k` (a, b)
    jam :: (a, a) `k` a 

-- Goal :: Construct some functions using the category vocabulary. 

class NumCat k a where 
    negateC :: a `k` a
    addC :: (a , a) `k` a
    mulC :: (a , a) `k` a
    divC :: (a , a) `k` a

instance Num a => NumCat (->) a where
    negateC :: Num a => a -> a
    negateC = negate
    addC :: Num a => (a, a) -> a
    addC = uncurry (+)  -- f(x,y) = x + y
    mulC :: Num a => (a, a) -> a
    mulC = uncurry (*)
    divC :: Num a => (a, a) -> a
    divC = mulC

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


instance Floating a => FloatCat (->) a where
    sinC :: Floating a => a -> a
    sinC = sin
    cosC :: Floating a => a -> a
    cosC = cos
    asinC :: Floating a => a -> a
    asinC = asin
    acosC :: Floating a => a -> a
    acosC = acos
    atanC :: Floating a => a -> a
    atanC = atan
    sinhC :: Floating a => a -> a
    sinhC = sinh
    coshC :: Floating a => a -> a
    coshC = cosh
    asinhC :: Floating a => a -> a
    asinhC = asinh
    acoshC :: Floating a => a -> a
    acoshC = acosh 
    atanhC :: Floating a => a -> a
    atanhC = atanh
    logC :: Floating a => a -> a
    logC = log
    expC :: Floating a => a -> a
    expC = exp

tri :: Cartesian k => k b c -> k b d -> k b (c, d)
tri f g = composition (cross f g) dup 

-- Examples of functions written in categorical vocabulary
sqr :: Num t => t -> t -- R -> R function
sqr = composition mulC (tri identity identity)

magSqr :: Num t => (t, t) -> t -- R^2 -> R function
magSqr = composition addC (tri (composition mulC (tri exl exl)) (composition mulC (tri exr exr)))

cosSinProd :: Floating t => (t, t) -> t
cosSinProd = composition mulC (composition (tri cosC sinC) mulC)

-- Until here, we can now see numbers which is a good thing.

-- Defining the newtype for differentiable functions itself
-- Making use of the fact that all linear maps are functions themselves at the end of the day.

-- Since linear maps have different representations, should I just declare them as a type union of 
-- all the possible forms a linear map could take ?

newtype D a b = D (a -> (b, a -> b))

linearD :: (a -> b) -> D a b
linearD f = D (\a -> (f a, f))

-- The derivative of a linear function is the function itself.

instance Category D where
    identity = linearD identity
    composition (D g) (D f) = D (\a -> let {(b, fdash) = f a; (c, gdash) = g b} in (c, composition gdash fdash))

instance Monoidal D where
    cross (D f) (D g) = D (\(a,b) -> let{(c, fdash) = f a; (d, gdash) = g b} in ((c, d), cross fdash gdash))

instance Cartesian D where
    exl = linearD exl
    exr = linearD exr
    dup = linearD dup
    