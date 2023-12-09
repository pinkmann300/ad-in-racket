{-# LANGUAGE MultiParamTypeClasses #-}

class Category k where 
  id :: a `k` a   
  -- Renamed id to ide to avoid confusion with the Prelude.GHC function id.
  (.) :: (b `k` c) -> (a `k` b) -> (a `k` c)
  -- Easier to use comp than the composition symbol itself. 

instance Category (->) where
  id = \a -> a 
  g . f = \a -> g(f a)

class Category k => Monoidal k where
  cross :: (a `k` c) -> (b `k` d) -> ((cross a b) `k` (cross c d))

instance Monoidal (->) where
  cross f g = \(m, n) -> cross (f m) (g n)

class Monoidal k => Cartesian k where
  exl :: (cross a b) `k` a
  exr :: (cross a b) `k` b
  dup :: a `k` (cross a a)

-- The above is a type class definition where k denoted in backticks refers to an infix type operator. 


