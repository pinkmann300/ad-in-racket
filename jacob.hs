
import GHC.Arr
import Data.Ix

-- Function to compute the Jacobian matrix of a given function
computeJacobian :: (Ix i, Num a) => (Array i a -> Array i a) -> Array i a -> Array (i, i) a
computeJacobian f arr =
  let n = rangeSize (bounds arr)
      m = rangeSize (bounds (f arr))
      indices = range (bounds arr)
      jacobianIndex = range ((0, 0), (m - 1, n - 1))
      jacobianEntry (i, j) = (f arr ! i ! j)
  in array ((0, 0), (m - 1, n - 1)) [((i, j), jacobianEntry (i, j)) | (i, j) <- jacobianIndex]

-- Example function: f(x, y) = [x^2, y^3]
exampleFunction :: (Num a, Ix i) => Array i (Array Int a) -> Array i (Array Int a)
exampleFunction arr = listArray (bounds arr) [listArray (bounds (arr ! 0)) [(arr ! 0 ! 0) ^ 2], listArray (bounds (arr ! 0)) [(arr ! 1 ! 0) ^ 3]]

-- Example usage
main :: IO ()
main = do
  let arr :: Array Int (Array Int Double)
      arr = listArray (0, 1) [listArray (0, 0) [1.0], listArray (0, 0) [2.0]] 

      jacobian = computeJacobian exampleFunction arr 
  putStrLn "Input point:"
  print arr
  putStrLn "Jacobian matrix:"
  print jacobian
