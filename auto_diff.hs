-- The below is an implementation of an automatic differentiation system where we treat differentials as a seperate component.

module Dual where

-- Beautiful tuple structure where it takes 2 elements of a given type. 
data Dual x = Dual x x deriving (Show)  

-- Spending some time with the structure itself.
p1 :: Dual Int 
p1 = Dual 2 2 

-- f returns the function part of the first element  
f :: Dual a -> a 
f (Dual x1 _) = x1

-- df returns the derivative part of the dual element.
df :: Dual a -> a 
df (Dual _ x2) = x2 



