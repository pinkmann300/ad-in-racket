import Language.Haskell.TH (tupleDataName)

data Tuple a = Cons a (Tuple a) | Empty
  deriving (Show, Read)

tupLength :: Tuple a -> Integer
tupLength (Cons k m) = 1 + tupLength m
tupLength Empty = 0

-- Do not really need a exr function as exln 2 will suffice.

exln :: Integer -> Tuple a -> a
exln _ Empty = error "Tuple Length out of bounds"
exln 1 (Cons k m) = k
exln n (Cons k m) = exln (n - 1) m


