liftA2 :: (t1 -> t2 -> t3) -> (t4 -> t1) -> (t4 -> t2) -> t4 -> t3
liftA2 h u v = \x -> h (u x) (v x)

class AdditiveGroup v where 
    zero :: v 
    addC :: v -> v -> v 
    negateC :: v -> v 

class AdditiveGroup v => Vector s v where 
    dot :: s -> v -> v 

instance AdditiveGroup v => AdditiveGroup (a -> v) where 
    zero = pure zero
    addC = liftA2 addC
    negateC = fmap negateC

instance Vector s v => Vector s (a -> v) where 
    dot s = fmap (dot s)

