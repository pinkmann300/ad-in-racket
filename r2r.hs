-- Recursive definition because we wish to compute higher order derivatives with the same chain.
newtype D x = D (x -> x, D x)

-- Cartesian functions
dup :: b -> (b, b)
dup a = (a, a)

exl (a, _) = a

exr (_, b) = b

-- Makes binary functions a unary function which takes a pair for an
scale = (*)

add = uncurry (+)

mul = uncurry scale

cross f g (a, b) = (f a, g b)

tri f g = cross f g . dup

downtri f g = add . cross f g

-- Derivative of a linear function is the function itself.

linearF f = \a -> (f a, f)

dIdentity = linearF id

dDup = linearF dup

dExl = linearF exl

dExr = linearF exr

dNegate = linearF negate

dAdd = linearF add

-- Parallel and Sequential composition of Differential functions

dCross f g k = ((c, d), cross fdash gdash)
  where
    ((c, fdash), (d, gdash)) = cross f g k

dComp g f a = (c, gdash . fdash)
  where
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

dSqr = dComp dMul dDup

--  Reference : https://crypto.stanford.edu/~blynn/haskell/ad.html

-- The above is an R2R implementation of forward mode automatic differentiation.
-- The above needs to be extended for reverse mode automatic differentiation.

rad d a = let (f, f') = d a in (f, (. f'))

-- f in here refers to the value of the function and the

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

duaxex :: Double -> (Double, Double -> Double)
duaxex = duaBin dMul <<. (dua dIdentity <</-\ dua dExp)

type Neuron = ([Double], Double)

fire :: (Floating a) => [a] -> ([a], a) -> a
fire inputs neuron = f $ sum (zipWith (*) inputs weights) + bias
  where
    weights = fst neuron
    bias = snd neuron
    f x = recip $ 1 + exp (-x)

jamList :: [Double] -> Double
jamList = sum

dJamList :: [Double] -> (Double, [Double] -> Double)
dJamList = linearF jamList

crossList :: [b -> c] -> [b] -> [c]
crossList = zipWith id

dCrossList :: [b1 -> (a, b2 -> c)] -> [b1] -> ([a], [b2] -> [c])
dCrossList f's = cross id crossList . unzip . crossList f's

dScale :: Double -> (Double -> Double, Double -> Double -> Double)
dScale = linearF scale

sigmoid = recip . add . (cross (const 1) (exp . negate)) . dup

fire' inputs = sigmoid . add . cross (jamList . crossList scaleInputs) id
  where
    scaleInputs = scale <$> inputs

-- RAD without any of the machinery present

dSinCosSqr2 = (((dSin <.) dCos <.) dMul <.) dDup