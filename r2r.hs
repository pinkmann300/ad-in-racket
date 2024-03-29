-- Recursive definition because we wish to compute higher order derivatives with the same chain.
newtype D x = D (x -> x, D x)

-- Cartesian functions
dup :: b -> (b, b)
dup a = (a, a)

exl :: (a, b) -> a
exl (a, _) = a

exr :: (a, b) -> b
exr (_, b) = b

-- Makes binary functions a unary function which takes a pair for an input
scale :: Double -> Double -> Double
scale = (*)

add :: (Double, Double) -> Double
add = uncurry (+)

mul :: (Double, Double) -> Double
mul = uncurry scale

-- Parallel composition of functions
cross :: (t1 -> a) -> (t2 -> b) -> (t1, t2) -> (a, b)
cross f g (a, b) = (f a, g b)

tri :: (t2 -> a) -> (t2 -> b) -> t2 -> (a, b)
tri f g = cross f g . dup

downtri :: (t1 -> Double) -> (t2 -> Double) -> (t1, t2) -> Double
downtri f g = add . cross f g

-- Derivative of a linear function is the function itself.

linearF :: (t -> a) -> t -> (a, t -> a)
linearF f = \a -> (f a, f)

dIdentity :: a -> (a, a -> a)
dIdentity = linearF id

dDup :: t -> ((t, t), t -> (t, t))
dDup = linearF dup

dExl :: (a, b) -> (a, (a, b) -> a)
dExl = linearF exl

dExr :: (a1, a2) -> (a2, (a1, a2) -> a2)
dExr = linearF exr

-- Only unary operation w.r.t to numeric operations and other binary derivatives
dNegate :: Integer -> (Integer, Integer -> Integer)
dNegate = linearF negate

dAdd :: (Double, Double) -> (Double, (Double, Double) -> Double)
dAdd = linearF add

-- Parallel and Sequential composition of Differential functions

dCross :: (t4 -> (a1, t5 -> a2)) -> (t6 -> (b1, t7 -> b2)) -> (t4, t6) -> ((a1, b1), (t5, t7) -> (a2, b2))
dCross f g k = ((c, d), cross fdash gdash)
  where
    ((c, fdash), (d, gdash)) = cross f g k

dComp :: (a1 -> (a2, b -> c)) -> (p -> (a1, a3 -> b)) -> p -> (a2, a3 -> c)
dComp g f a = (c, gdash . fdash)
  where
    (b, fdash) = f a
    (c, gdash) = g b

dMul :: (Double, Double) -> (Double, (Double, Double) -> Double)
dMul (a, b) = (a * b, downtri (scale a) (scale b))

prodRu :: (t2 -> a) -> (t2 -> Double) -> t2 -> (a, Double -> Double)
prodRu f fdash = tri f (scale . fdash)

dExp :: Double -> (Double, Double -> Double)
dExp = prodRu exp exp

dLog :: Double -> (Double, Double -> Double)
dLog = prodRu log recip

dSin :: Double -> (Double, Double -> Double)
dSin = prodRu sin cos

dCos :: Double -> (Double, Double -> Double)
dCos = prodRu cos (negate . sin)

dAsin :: Double -> (Double, Double -> Double)
dAsin = prodRu asin (\x -> recip (sqrt (1 - (x * x))))

dAcos :: Double -> (Double, Double -> Double)
dAcos = prodRu acos (\x -> recip (sqrt (1 - (x * x))))

dAtan :: Double -> (Double, Double -> Double)
dAtan = prodRu atan (\x -> recip (x * x + 1))

dSinh :: Double -> (Double, Double -> Double)
dSinh = prodRu sinh cosh

dCosh :: Double -> (Double, Double -> Double)
dCosh = prodRu cosh sinh

dAsinh :: Double -> (Double, Double -> Double)
dAsinh = prodRu asinh (\x -> recip (sqrt (x * x + 1)))

dAcosh :: Double -> (Double, Double -> Double)
dAcosh = prodRu acosh (\x -> negate (recip (sqrt (x * x - 1))))

dAtanh :: Double -> (Double, Double -> Double)
dAtanh = prodRu atanh (\x -> recip (1 - x * x))

-- Example functions defined using categorical vocabulary:
dSqr :: Double -> (Double, Double -> Double)
dSqr = dComp dMul dDup

--  Reference : https://crypto.stanford.edu/~blynn/haskell/ad.html

-- The above is an R2R implementation of forward mode automatic differentiation.
-- The above needs to be extended for reverse mode automatic differentiation.

rad d a = let (f, f') = d a in (f, (. f'))

infixr 9 <<.

g <<. f = \a ->
  let (b, f0) = f a
      (c, g0) = g b
   in (c, f0 . g0)

(g <. f) a = (c, g0 . f0)
  where
    (b, f0) = f a
    (c, g0) = g b

radSqr = rad dMul <<. rad dDup

radSinCosSqr = rad dSin <<. rad dCos <<. radSqr

inl a = (a, 0)

inr b = (0, b)

jam = uncurry (+)

join (f, g) = jam . cross f g

unjoin h = (h . inl, h . inr)

radCross f g ab =
  let ((c, f'), (d, g')) = cross f g ab
   in ((c, d), join . cross f' g' . unjoin)

radCosSinProd = (radCross (rad dCos) (rad dSin) <<. rad dDup) <<. rad dMul

radMagSqr = rad dAdd <<. radCross (rad dMul <<. rad dDup) (rad dMul <<. rad dDup)

radDer f x = snd (f x) id 1

dot = scale

undot = ($ 1)

dot2 (u, v) = downtri (dot u) (dot v)

undot2 f = (f (1, 0), f (0, 1))

dua d a = let (f, f') = d a in (f, undot . (. f') . dot)

duaBin d a = let (f, f') = d a in (f, undot2 . (. f') . dot)

duaDup a = (dup a, jam)

duaJam a = (jam a, dup)

duaScale s a = (scale s a, scale s)

duaCross = dCross

duaSqr = duaBin dMul <<. duaDup

duaMagSqr = duaBin dAdd <<. dCross (duaBin dMul <<. duaDup) (duaBin dMul <<. duaDup)

f <</-\ g = duaCross f g <<. duaDup

duaxex = duaBin dMul <<. (dua dIdentity <</-\ dua dExp)

type Neuron = ([Double], Double)

fire inputs neuron = f $ sum (zipWith (*) inputs weights) + bias
  where
    weights = fst neuron
    bias = snd neuron
    f x = recip $ 1 + exp (-x)

jamList = sum

dJamList = linearF jamList

crossList = zipWith id

dCrossList f's = cross id crossList . unzip . crossList f's

dScale = linearF scale

sigmoid = recip . add . (cross (const 1) (exp . negate)) . dup

fire' inputs = sigmoid . add . cross (jamList . crossList scaleInputs) id
  where
    scaleInputs = scale <$> inputs
