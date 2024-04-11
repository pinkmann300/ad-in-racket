-- Cartesian functions
dup :: b -> (b, b)
dup a = (a, a)

exl :: (a, b) -> a
exl (a, _) = a

exr :: (a, b) -> b
exr (_, b) = b

scale :: (Num t) => t -> t -> t
-- Makes binary functions a unary function which takes a pair for an
scale = (*)

add :: (Num t) => (t, t) -> t
add = uncurry (+)

mul :: (Num t) => (t, t) -> t
mul = uncurry scale

cross :: (t1 -> a) -> (t2 -> b) -> (t1, t2) -> (a, b)
cross f g (a, b) = (f a, g b)

tri :: (t2 -> a) -> (t2 -> b) -> t2 -> (a, b)
tri f g = cross f g . dup

downtri :: (Num a) => (t1 -> a) -> (t2 -> a) -> (t1, t2) -> a
downtri f g = add . cross f g

-- Derivative of a linear function is the function itself.
-- <. is the normal composition function.
(<.) :: (fout -> (gout, fout -> gout)) -> (fin -> (fout, fin -> fout)) -> fin -> (gout, fin ->  gout)
(g <. f) a = (c, g0 . f0)
  where
    (b, f0) = f a
    (c, g0) = g b

linearF :: (t -> a) -> t -> (a, t -> a)
linearF f a = (f a, f)

dIdentity :: a -> (a, a -> a)
dIdentity = linearF id

dDup :: t -> ((t, t), t -> (t, t))
dDup = linearF dup

dExl :: (a, b) -> (a, (a, b) -> a)
dExl = linearF exl

dExr :: (a1, a2) -> (a2, (a1, a2) -> a2)
dExr = linearF exr

dNegate :: (Num t) => t -> (t, t -> t)
dNegate = linearF negate

dAdd :: (Num t) => (t, t) -> (t, (t, t) -> t)
dAdd = linearF add

-- Parallel and Sequential composition of Differential functions

dMul :: (Num a) => (a, a) -> (a, (a, a) -> a)
dMul (a, b) = (a * b, downtri (scale a) (scale b))

prodRu :: (Num b) => (t2 -> a) -> (t2 -> b) -> t2 -> (a, b -> b)
prodRu f fdash = tri f (scale . fdash)

dExp :: (Floating t) => t -> (t, t -> t)
dExp = prodRu exp exp

dLog :: (Floating t) => t -> (t, t -> t)
dLog = prodRu log recip

dSin :: (Floating t) => t -> (t, t -> t)
dSin = prodRu sin cos

dCos :: (Floating t) => t -> (t, t -> t)
dCos = prodRu cos (negate . sin)

dAsin :: (Floating t) => t -> (t, t -> t)
dAsin = prodRu asin (\x -> recip (sqrt (1 - (x * x))))

dAcos :: (Floating t) => t -> (t, t -> t)
dAcos = prodRu acos (\x -> recip (sqrt (1 - (x * x))))

dAtan :: (Floating t) => t -> (t, t -> t)
dAtan = prodRu atan (\x -> recip (x * x + 1))

dSinh :: (Floating t) => t -> (t, t -> t)
dSinh = prodRu sinh cosh

dCosh :: (Floating t) => t -> (t, t -> t)
dCosh = prodRu cosh sinh

dAsinh :: (Floating t) => t -> (t, t -> t)
dAsinh = prodRu asinh (\x -> recip (sqrt (x * x + 1)))

dAcosh :: (Floating t) => t -> (t, t -> t)
dAcosh = prodRu acosh (\x -> negate (recip (sqrt (x * x - 1))))

dAtanh :: (Floating t) => t -> (t, t -> t)
dAtanh = prodRu atanh (\x -> recip (1 - x * x))

dSqr :: (Floating t) => t -> (t, t -> t)
dSqr = dComp dMul dDup

--  Reference : https://crypto.stanford.edu/~blynn/haskell/ad.html

-- The above is an R2R implementation of forward mode automatic differentiation.
-- The above needs to be extended for reverse mode automatic differentiation.

-- f in here refers to the value of the function and the
infixr 9 <<.

dComp :: (fout -> (gout, fout -> gout)) -> (fin -> (fout, fin -> fout)) -> fin -> (gout, fin -> gout)
dComp g f a = (c, gdash . fdash)
  where
    (b, fdash) = f a
    (c, gdash) = g b

dCross :: (fin -> (fout, fin -> fout)) -> (gin -> (gout, gin -> gout)) -> (fin, gin) -> ((fout, gout), (fin, gin) -> (fout, gout))
dCross f g k = ((c, d), cross fdash gdash)
  where
    ((c, fdash), (d, gdash)) = cross f g k

-- <<. is the composition function.
(<<.) :: (forall c. (fout -> (gout, (gout -> c) -> fout -> c)) -> (fin -> (fout , (fout -> c) -> fin -> c)) -> fin -> (gout, (gout -> c) -> fin -> c))
g <<. f = \a ->
  let (b, f0) = f a
      (c, g0) = g b
   in (c, f0 . g0)

rad :: (forall c. (fin -> (fout, fin -> fout)) -> fin -> (fout, (fout -> c) -> fin -> c))
rad f a = let (fval, f') = f a in (fval, (. f'))

-- g0 :: (gout -> c) -> fout -> c 
-- f0 :: (fout -> c) -> fin -> c 

-- f0.g0 :: ? Type computation doubt

inl a = (a, 0)

inr b = (0, b)


jam = uncurry (+)

join (f, g) = jam . cross f g
unjoin h = (h . inl, h . inr)

--radCross :: (Num c1, Num b1, Num a1) => (t4 -> (a2, (a1 -> c2) -> t5 -> c1)) -> (t6 -> (b2, (b1 -> c2) -> t7 -> c1)) -> (t4, t6) -> ((a2, b2), ((a1, b1) -> c2) -> (t5, t7) -> c1)
--radCross :: (fin -> (fout , fin -> fout)) -> (gin -> (gout, gin -> gout)) -> (fin, gin) -> ((fout, gout), ()

radCross f g ab = 
  let ((c, f'), (d, g')) = cross f g ab
   in ((c, d), join . cross f' g' . unjoin)

radCosSinProd :: (Double, Double) -> ((Double, Double), ((Double, Double) -> Integer) -> (Double, Double) -> Integer)
radCosSinProd = (radCross (rad dCos) (rad dSin) <<. rad dDup) <<. rad dMul

radMagSqr = rad dAdd <<. radCross (rad dMul <<. rad dDup) (rad dMul <<. rad dDup)

radDer f x = snd (f x) id 1

-- dot = scale

-- undot = ($ 1)

-- dot2 (u, v) = downtri (dot u) (dot v)

-- undot2 f = (f (1, 0), f (0, 1))

-- dua d a = let (f, f') = d a in (f, undot . (. f') . dot)

-- duaBin d a = let (f, f') = d a in (f, undot2 . (. f') . dot)

-- duaDup a = (dup a, jam)

-- duaJam a = (jam a, dup)

-- duaScale s a = (scale s a, scale s)

-- duaCross = dCross

-- duaSqr = duaBin dMul <<. duaDup

-- duaMagSqr = duaBin dAdd <<. dCross (duaBin dMul <<. duaDup) (duaBin dMul <<. duaDup)

-- f <</-\ g = duaCross f g <<. duaDup

-- duaxex = duaBin dMul <<. (dua dIdentity <</-\ dua dExp)

-- type Neuron = ([Double], Double)

-- fire :: (Floating a) => [a] -> ([a], a) -> a
-- fire inputs neuron = f $ sum (zipWith (*) inputs weights) + bias
--   where
--     weights = fst neuron
--     bias = snd neuron
--     f x = recip $ 1 + exp (-x)

-- jamList :: [Double] -> Double
-- jamList = sum

-- dJamList :: [Double] -> (Double, [Double] -> Double)
-- dJamList = linearF jamList

-- crossList :: [b -> c] -> [b] -> [c]
-- crossList = zipWith id

-- dCrossList :: [b1 -> (a, b2 -> c)] -> [b1] -> ([a], [b2] -> [c])
-- dCrossList f's = cross id crossList . unzip . crossList f's

-- dScale :: Double -> (Double -> Double, Double -> Double -> Double)
-- dScale = linearF scale

-- sigmoid = recip . add . cross (const 1) (exp . negate) . dup

-- fire' inputs = sigmoid . add . cross (jamList . crossList scaleInputs) id
--   where
--     scaleInputs = scale <$> inputs

-- -- RAD without any of the machinery present

-- dSinCosSqr2 = dSin <. dCos <. dMul <. dDup