-- The typed version of the scalar domain program for first order
-- derivatives.
-- One improvement which has been ignored is the ignorance of zeros
-- in the derivative computation.
{-# LANGUAGE MonoLocalBinds #-}

data D a = D a a deriving (Show, Read)

liftA2 :: (t1 -> t2 -> t3) -> (t4 -> t1) -> (t4 -> t2) -> t4 -> t3
liftA2 h u v = \x -> h (u x) (v x)

instance (Num b) => Num (a -> b) where
  fromInteger :: (Num b) => Integer -> a -> b
  fromInteger = pure . fromInteger
  negate :: (Num b) => (a -> b) -> a -> b
  negate = fmap negate
  (+) :: (Num b) => (a -> b) -> (a -> b) -> a -> b
  (+) = liftA2 (+)
  (*) :: (Num b) => (a -> b) -> (a -> b) -> a -> b
  (*) = liftA2 (*)
  abs :: (Num b) => (a -> b) -> a -> b
  signum :: (Num b) => (a -> b) -> a -> b
  abs = fmap abs
  signum = fmap signum

instance (Fractional b) => Fractional (a -> b) where
  fromRational :: (Fractional b) => Rational -> a -> b
  fromRational = pure . fromRational
  recip :: (Fractional b) => (a -> b) -> a -> b
  recip = fmap recip

instance (Floating b) => Floating (a -> b) where
  pi = pure pi
  sqrt = fmap sqrt
  exp = fmap exp
  log = fmap log
  sin = fmap sin
  cos = fmap cos
  asin = fmap asin
  acos = fmap acos
  atan = fmap atan
  sinh = fmap sinh
  cosh = fmap cosh
  asinh = fmap asinh
  acosh = fmap acosh
  atanh = fmap atanh

-- For R2R implementations the chain rule is just a multiplication of the
-- number derivatives themselves.

(<**>) :: (Num a) => (a -> a) -> (a -> a) -> (D a -> D a)
(f <**> f') (D a a') = D (f a) (a' * (f' a))

constD :: (Num a) => a -> D a
constD x = D x 0

idD :: (Num a) => a -> D a
idD x = D x 1

instance (Num a) => Num (D a) where
  fromInteger = constD . fromInteger
  D x x0 + D y y0 = D (x + y) (x0 + y0)
  D x x' * D y y' = D (x * y) (y' * x + y * x')
  negate = negate <**> (-1)
  signum = signum <**> 0
  abs = abs <**> signum

sqr :: (Num a) => a -> a
sqr x = x * x

instance (Fractional a) => Fractional (D a) where
  fromRational = constD . fromRational
  recip = recip <**> (-sqr recip)

instance (Floating x) => Floating (D x) where
  pi = constD pi
  exp = exp <**> exp
  log = log <**> recip
  sqrt = sqrt <**> recip (2 * sqrt)
  sin = sin <**> cos
  cos = cos <**> (-sin)
  asin = asin <**> recip (sqrt (1 - sqr))
  acos = acos <**> recip (-sqrt (1 - sqr))
  atan = atan <**> recip (1 + sqr)
  sinh = sinh <**> cosh
  cosh = cosh <**> sinh
  asinh = asinh <**> recip (sqrt (1 + sqr))
  acosh = acosh <**> recip (-sqrt (sqr - 1))
  atanh = atanh <**> recip (1 - sqr)

f1 :: (Floating a) => a -> a
f1 z = sqrt (3 * sin z)