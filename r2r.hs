-- Recursive definition because we wish to compute higher order derivatives with the same chain.
newtype D x = D (x -> x, D x)

-- Cartesian functions
dup a = (a, a)

exl (a, _) = a

exr (_ ,b) = b

-- Makes binary functions a unary function which takes a pair for an input
scale = (*)

add = uncurry (+)

mul = uncurry scale

-- Parallel composition of functions 
cross f g (a,b) = (f a, g b)

tri f g = cross f g . dup

downtri f g = add . cross f g

-- Derivative of a linear function is the function itself.

linearF f = \a -> (f a, f)

dIdentity = linearF id

dDup = linearF dup

dExl = linearF exl

dExr = linearF exr

-- Only unary operation w.r.t to numeric operations and other binary derivatives
dNegate = linearF negate

dAdd = linearF add

-- Parallel and Sequential composition of Differential functions

dCross f g k = ((c, d), cross fdash gdash) where
    ((c,fdash), (d, gdash)) =  cross f g k

dComp g f a = (c, gdash . fdash) where
    (b, fdash) = f a
    (c, gdash) = g b

dMul (a, b) = (a * b, downtri (scale a) (scale b))

prodRu f fdash = tri f (scale . fdash)

dExp = prodRu exp exp

dLog = prodRu log recip

dSin = prodRu sin cos

dCos = prodRu cos (negate . sin)

dAsin = prodRu asin (\x -> recip (sqrt (1 - (x * x))))

dAcos = prodRu acos (\x -> recip (sqrt (1 - (x * x))))

dAtan = prodRu atan (\x -> recip (x * x + 1))

dSinh = prodRu sinh cosh

dCosh = prodRu cosh sinh

dAsinh = prodRu asinh (\x -> recip (sqrt (x * x + 1)))

dAcosh = prodRu acosh (\x -> negate (recip (sqrt (x * x - 1))))

dAtanh = prodRu atanh (\x -> recip (1 - x * x))

-- Example functions defined using categorical vocabulary: 
dSqr = dComp dMul dDup


--  Reference : https://crypto.stanford.edu/~blynn/haskell/ad.html


