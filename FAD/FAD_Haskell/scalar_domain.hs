-- Very similar to the Dual Numbers approach.
-- Works only for functions from R -> R type.
-- Only allowed to compute the first derivative. 
-- Not more than that. 

data D a = D a a deriving (Show, Eq)

constD :: (Num a) => a -> D a
constD x = D x 0

idD :: (Num a) => a -> D a
idD x = D x 1

sqr :: (Num a) => a -> a
sqr x = x * x

instance (Num a) => Num (D a) where
  fromInteger x = constD (fromInteger x)
  D x x' + D y y' = D (x + y) (x' + y')
  D x x' * D y y' = D (x * y) (y' * x + y * x')
  negate (D x x') = D (negate x) (negate x')
  signum (D x _) = D (signum x) 0
  abs (D x x') = D (abs x) (x' * signum x)

instance (Fractional x) => Fractional (D x) where
  fromRational x = constD (fromRational x)
  recip (D x x') = D (recip x) (-x' / sqr x)

instance (Floating x) => Floating (D x) where
  pi = constD pi
  exp (D x x') = D (exp x) (x' * exp x)
  log (D x x') = D (log x) (x' / x)
  sqrt (D x x') = D (sqrt x) (x' / (2 * sqrt x))
  sin (D x x') = D (sin x) (x' * cos x)
  cos (D x x') = D (cos x) (x' * (-sin x))
  asin (D x x') = D (asin x) (x' / sqrt (1 - sqr x))
  acos (D x x') = D (acos x) (x' / (-sqrt (1 - sqr x)))
  atan (D x x') = D (atan x) (x' / (1 + sqr x))
  sinh (D x x') = D (sinh x) (x' * cosh x)
  cosh (D x x') = D (cosh x) (x' * sinh x)
  asinh (D x x') = D (asinh x) (x' / sqrt (1 + sqr x))
  acosh (D x x') = D (acosh x) (x' / sqrt (sqr x - 1))
  atanh (D x x') = D (atanh x) (x' / (1 + sqr x))

f1 :: (Floating a) => a -> a
f1 z = sqrt (3 * sin z)


