{-# HLINT ignore "Use tuple-section" #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
import qualified Control.Arrow as Control

-- Add more instances for other types as needed

class Category k where
  identity :: a `k` a
  composition :: (b `k` l) -> (a `k` b) -> (a `k` l)

instance Category (->) where
  identity :: a -> a
  identity = id
  composition :: (b -> c) -> (a -> b) -> a -> c
  composition g f = g . f

class (Category k) => Monoidal k where
  cross :: (a `k` c) -> (b `k` d) -> ((a, b) `k` (c, d))

class (Monoidal k) => Cartesian k where
  exl :: (a, b) `k` a
  exr :: (a, b) `k` b
  dup :: a `k` (a, a)

instance Monoidal (->) where
  cross :: (a -> k) -> (b -> d) -> ((a, b) -> (k, d))
  cross f g (a, b) = (f a, g b)

instance Cartesian (->) where
  exl :: (a, b) -> a
  exl = fst
  exr :: (a, b) -> b
  exr = snd
  dup :: a -> (a, a)
  dup a = (a, a)

linearD :: (a -> b) -> D a b
linearD f = D (\a -> (f a, (. f)))

newtype D a b = D (forall c. Num c => a -> (b, (b -> c) -> a -> c))

instance Category D where
  identity = linearD id
  composition (D g) (D f) = D (\a -> let (b, f') = f a; (c , g') = g b in (c, f' . g'))

instance Monoidal D where
  cross :: D a c -> D b d -> D (a, b) (c, d)
  cross (D f) (D g) = D (\(a, b) -> let (fa, fa') = f a; (gb, gb') = g b in ((fa,gb), join . cross fa' gb' . unjoin))


join :: Num c => (a -> c, b -> c) -> (a, b) -> c
join (f, g) (a, b) = f a + g b