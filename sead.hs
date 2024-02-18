-- R -> R function implementation 

newtype D x = D (x -> x, D x)

-- Recursive definition of the data type D where the first part corresponds to the function under consideration and the second part 
-- is another function which is differentiable. This captures the idea of the datatype differentiable functions in Conal implementation.

-- For ease of extraction and testing, we write 2 functions to extract the function and the differentiable function out of the datatype.

fun :: D x -> (x -> x)
fun (D (f, _ )) = f

dfun :: D x -> D x
dfun (D (_, df)) = df

-- Special identity function 
x :: (Real a) => D a   
x = D (id, 1)

-- Now, we jump into how these functions can be formed and use the same to compute the derivatives. Starting off with the Num class case
-- we consider these 3 cases :

        -- D (f + g) = D f + D g
        -- D (- g) = - D g
        -- D (f x g) = f D g + g D f

-- Capturing these 3 cases, we declare instance 

instance Num n => Num (D n) where
    negate f = D (negate . fun f, negate (dfun f))
    f + g = D (\a -> fun f a + fun g a, dfun f + dfun g)
    f * g = D (\a -> fun f a * fun g a, f * dfun g + g * dfun f)
    abs f = D (abs . fun f, (dfun f) * signum f)
    signum f = D (signum . fun f, 0)                                -- Think of as a base case (Non-linear primitive)
    fromInteger m = D (\_ -> fromInteger m, 0)                      -- Derivative of a constant will always be 0 (Non-linear primitive)


-- Some concepts which are not captured are the division rule which involves 
-- subtraction of some components . D (f(x) / g(x)) 

instance (Fractional n) => Fractional (D n) where
    fromRational a = D (\_ -> fromRational a, 0)
    recip f = D (\x -> 1 / fun f x, (- dfun f / (f * f)))

instance (Floating n) => Floating (D n) where
    pi = D (\_ -> pi, 0)
    exp f = D (exp . fun f, dfun f * (exp f))
    log f = D (log . fun f, dfun f / f)
    sin f = D (sin . fun f, dfun f * cos f)
    cos f = D (cos . fun f, -(dfun f * cos f))
    asin f = D (asin . fun f, (dfun f / sqrt (1 - f * f)))
    acos f = D (acos . fun f, (- dfun f / sqrt (1 - f * f)))
    atan f = D (atan . fun f, dfun f / (1 + f * f))
    sinh f = D (sinh. fun f, dfun f * cosh f)
    cosh f = D (cosh . fun f, dfun f * sinh f)
    asinh f = D (asinh . fun f, ((dfun f) / sqrt (1 + f * f)))
    acosh f = D (acosh . fun f, dfun f / sqrt (f * f -1))
    atanh f = D (atanh . fun f, dfun f / (1 + f * f))

-- That completes the R -> R implementation. Now, we need to get a R^n -> R^m where n and m are arbitrary integers.
-- We are not concerned about the C -> C case because Reals are a little more easier to handle than complex integers.

newtype DE a b = DE (a -> b, DE a b)

fun1 :: DE a b -> (a -> b)
fun1 (DE (f, _)) = f

dfun1 :: DE a b -> DE a b
dfun1 (DE (_, df)) = df

-- Examples of functions written in more than one variable

-- Design decisions - how to decide what dimension the input vector belongs to 
-- (Treat it as a function on an array and the length of the array will correspond to the dimension
-- of the function.






