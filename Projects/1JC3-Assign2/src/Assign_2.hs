  {- Assignment 2
  - Name: Tahseen Ahmed
  - Date: 06/10/2018 - 18/21/2018
  - Version 1.0.0 - All tests complete. Should be working well.
  - Included some quickCheck cases to test.
  -}
  module Assign_2 where

  import Data.List
  import Data.Maybe
  import Test.QuickCheck

  macid :: String
  macid = "ahmedt26"

  type Vector = (Double,Double,Double)

  {- -----------------------------------------------------------------
  - vectors, vectors1 and testvec
  - -----------------------------------------------------------------
  - These are test inputs I used to start testing my code. It's easier to
  type these function names in rather than the entire vector list.
  -}
  vectors :: [Vector]
  vectors = [ (1.0,2.0,3.0), (2.0,3.0,4.0), (4.0,3.0,5.0)]

  vectors1 :: [Vector]
  vectors1 = [ (8.0,9.0,56.0) , (-7.0, 3.0, 5.0) , (211.0, 0.0, 0.0)]

  vectors2 :: [Vector]
  vectors2 = [ (1.0,2.0,3.0) , (-5.0,-6.0,-7.0), (-3.0,-2.0,-1.0)]

  testvec :: Vector
  testvec = (1.0,0.0,0.0)

  {- -----------------------------------------------------------------
  - callx, cally and callz
  - -----------------------------------------------------------------
  - These functions call the x, y and z componenets of of a given vector.
  They are designed to be used for vectors only.
  -}

  callx :: Vector -> Double
  callx (a,_,_) = a

  cally :: Vector -> Double
  cally (_,a,_) = a

  callz :: Vector -> Double
  callz (_,_,a) = a

  {- -----------------------------------------------------------------
  - vecZero
  - -----------------------------------------------------------------
  - Returns the zero vector. A vector with no magnitude.
  -}
  vecZero :: Vector
  vecZero = (0.0,0.0,0.0)

  {- -----------------------------------------------------------------
  - vecScalarProd
  - -----------------------------------------------------------------
  - Multiplies a vector by a scalar multiple.
  -}
  vecScalarProd :: Double -> Vector -> Vector
  vecScalarProd x v0 = (x * callx v0, x * cally v0, x * callz v0)

  {- -----------------------------------------------------------------
  - vecSum
  - -----------------------------------------------------------------
  - Adds two vectors together to compute the vector sum.
  -}
  vecSum :: Vector -> Vector -> Vector
  vecSum v0 v1 = (x + x', y + y', z + z')
      where
          x = callx v0
          y = cally v0
          z = callz v0
          x' = callx v1
          y' = cally v1
          z' = callz v1

  {- -----------------------------------------------------------------
  - vecMagnitude
  - -----------------------------------------------------------------
  - Computes the magnitude (length) of a vector.
  -}
  vecMagnitude :: Vector -> Double
  vecMagnitude v0 = sqrt $ callx v0 ** 2 + cally v0 ** 2 + callz v0 ** 2

  {- -----------------------------------------------------------------
  - vecInnerProd
  - -----------------------------------------------------------------
  - Computes the dot product of two vectors. The dot product (inner product) is
  a scalar value that is the cosine of the angle between two vectors.
  -}
  vecInnerProd :: Vector -> Vector -> Double
  vecInnerProd v0 v1 =  (x * x') + (y * y') + (z * z')
      where
          x = callx v0
          y = cally v0
          z = callz v0
          x' = callx v1
          y' = cally v1
          z' = callz v1

  {- -----------------------------------------------------------------
  - vecDistBtwn
  - -----------------------------------------------------------------
  - Computes the distance between one vector and another. Alternatively,
  computes the distance between one point in space and another.
  -}
  vecDistBtwn :: Vector -> Vector -> Double
  vecDistBtwn v0 v1 = vecMagnitude vd
      where
        vd = vecSum v0 $ vecScalarProd (-1.0)v1
        -- vd is the difference vector, v0 - v1.
        -- The distance between two vectors is the magitude of v0 - v1.

  {- -----------------------------------------------------------------
  - vecDistList
  - -----------------------------------------------------------------
  - Computes the distances between a vector and  each vector in the given list
  and outputs a list of distances. so [v1,v2,v3] -> [d1,d2,d3].
  -}
  vecDistList :: Vector -> [Vector] -> [Double]
  vecDistList v0 vs
      | null vs = [0.0] -- A case to prevent error when an empty list is provided.
      | otherwise = map ( vecDistBtwn v0 ) vs


  {- -----------------------------------------------------------------
  - vecFindMin
  - -----------------------------------------------------------------
  - Returns the element position of the minimum value in the list of distances.
  It's used to cross-reference the vector that provided that minimum distance
  in the vector list.
  -}
  vecFindMin :: [Double] -> Maybe Int
  vecFindMin ds = elemIndex (minimum ds) ds

  {- -----------------------------------------------------------------
  - vecFindMax
  - -----------------------------------------------------------------
  - Returns the element position of the maximum value in the list of distances.
  It's used to cross-reference the vector that provided that maximum distance
  in the vector list.
  -}
  vecFindMax :: [Double] -> Maybe Int
  vecFindMax ds = elemIndex (maximum ds) ds
  {- vecFindMin/Max will return nothing when the list is empty, but that's not
  an issue as vecF deals with that case.
  -}


  {- -----------------------------------------------------------------
  - vecF
  - -----------------------------------------------------------------
  - Compares vector v0 to a list of vectors vs and finds the closest and farthest
  vector to v0 in the list. If there is no list given, vecF outputs the origin
  as the closest and farthest vector from v0.
  -}
  vecF :: Vector -> [Vector] -> (Vector,Vector)
  vecF v0 vs
    | null vs = (vecZero,vecZero) -- null vs just means an empty vector list.
    | otherwise = ( vs !! x , vs !! y )
        where
          x = fromJust $ vecFindMin (vecDistList v0 vs)
          y = fromJust $ vecFindMax (vecDistList v0 vs)
        {- Since vecFindMin/Max both return a Maybe Int, the fromJust function
        from Data.Maybe is used to convert that back to an integer.
        -}

  {- -----------------------------------------------------------------
  - QuickCheck Propositions
  - -----------------------------------------------------------------
  - This is a list of propositions which must be true for every input of each
  function. Call QuickCheck with each of these to test each function.
  -}

  vecScalarProdProp :: Double -> Vector -> Bool
  vecScalarProdProp _ vecZero = True
  vecScalarPropdProp x v0 = (x - tol) < ratio && ratio < (x + tol)
      where
        ratio = vecMagnitude (vecScalarProd x v0) / vecMagnitude v0
        tol = 1.0e-4
  {- The ratio between the scalar-multiplied magnitude and the original magnitude
  always equals the scalar multiple.
  This rule only applies when the scalar /= 0.0 and the vector is not the zero vector.
  If the scalar multiple was 2, then 2d/d = 2 where d is the magnitude of the orignal vector.
  The zero vector multiplied by ANY multiple will always return the zero vector.
  The latter case can't be added into the original rule as the computation
  results in a 0/0 divison, which is indeterminate.
  The tolerance is added because of floating point arithmetic errors.
  -}

  vecMagnitudeProp :: Vector -> Bool
  vecMagnitudeProp v0 = vecMagnitude v0 >= 0
  -- Vector magnitudes are never negative as you can't have negative length.

  vecDistBtwnProp :: Vector -> Vector -> Bool
  vecDistBtwnProp v0 v1 = vecDistBtwn v0 v1 >= 0.0
  -- A distance between one point and another is absolute, there is no negative.

  {- -----------------------------------------------------------------
  - Test Cases
  - -----------------------------------------------------------------
  -
  - -----------------------------------------------------------------
  - Tests for: callx, cally and callz
  - -----------------------------------------------------------------
  - - Function: callx
  - - Test Case Number: 0
  - - Input: (1.0 , 2.0 , 3.0)
  - - Expected Output: 1.0
  - - Acutal Output: 1.0
  - -----------------------------------------------------------------
  - -----------------------------------------------------------------
  - - Function: cally
  - - Test Case Number: 0
  - - Input: (1.0 , 2.0 , 3.0)
  - - Expected Output: 2.0
  - - Acutal Output: 2.0
  - -----------------------------------------------------------------
  - -----------------------------------------------------------------
  - - Function: callz
  - - Test Case Number: 0
  - - Input: (1.0 , 2.0 , 3.0)
  - - Expected Output: 3.0
  - - Acutal Output: 3.0
  -
  - -----------------------------------------------------------------
  - Tests for: vecScalarProd
  - -----------------------------------------------------------------
  - - Function: vecScalarProd
  - - Test Case Number: 0
  - - Input: 2.0 (1.0, 2.0, 3.0)
  - - Expected Output: (2.0, 4.0, 6.0)
  - - Acutal Output: (2.0, 4.0, 6.0)
  - -----------------------------------------------------------------
  - -----------------------------------------------------------------
  - - Function: vecScalarProd
  - - Test Case Number: 1
  - - Input: -2.0 (1.0, -2.0, 3.0)
  - - Expected Output: (-2.0, 4.0, -6.0)
  - - Acutal Output: (-2.0, 4.0, -6.0)
  - -----------------------------------------------------------------
  - -----------------------------------------------------------------
  - - Function: vecScalarProd
  - - Test Case Number: 2
  - - Input: 0.0 (1.0, 2.0, 3.0)
  - - Expected Output: (0.0, 0.0 , 0.0)
  - - Acutal Output: (0.0, 0.0 , 0.0)
  - -----------------------------------------------------------------
  - Tests for: vecSum
  - -----------------------------------------------------------------
  - - Function: vecSum
  - - Test Case Number: 0
  - - Input: (1.0, 2.0, 3.0) (3.0, 2.0, 1.0)
  - - Expected Output: (4.0, 4.0, 4.0)
  - - Acutal Output: (4.0, 4.0, 4.0)
  - -----------------------------------------------------------------
  - -----------------------------------------------------------------
  - - Function: vecSum
  - - Test Case Number: 1
  - - Input: (0.0, 0.0, 0.0) (1.0, 2.0, 3.0)
  - - Expected Output: (1.0, 2.0, 3.0)
  - - Acutal Output: (1.0, 2.0, 3.0)
  - -----------------------------------------------------------------
  - -----------------------------------------------------------------
  - - Function: vecSum
  - - Test Case Number: 2
  - - Input: (200.5, -100.23, 12.34) (-200.5, 100.23, -12.34)
  - - Expected Output: (0.0, 0.0, 0.0)
  - - Acutal Output: (0.0, 0.0, 0.0)
  - -----------------------------------------------------------------
  - Tests for: vecMagnitude
  - -----------------------------------------------------------------
  - - Function: vecMagnitude
  - - Test Case Number: 0
  - - Input: (0.0, 0.0, 0.0)
  - - Expected Output: 0.0
  - - Acutal Output: 0.0
  - -----------------------------------------------------------------
  - -----------------------------------------------------------------
  - - Function: vecMagnitude
  - - Test Case Number: 1
  - - Input: (-30.12, 23.12, 1212.00912)
  - - Expected Output: 1212.60375051
  - - Acutal Output:   1212.6037505150534
  - - The calculator can only go up to so many digits.
  - -----------------------------------------------------------------
  - -----------------------------------------------------------------
  - - Function: vecMagnitude
  - - Test Case Number: 2
  - - Input: (0.00012, 0.0009, 0.0005005)
  - - Expected Output: 1.036773963e-3
  - - Acutal Output:   1.0367739628289283e-3
  - - Miniscule discrepancy caused by floating point error.
  - -----------------------------------------------------------------
  - Tests for: vecInnerProd
  - -----------------------------------------------------------------
  - - Function: vecInnerProd
  - - Test Case Number: 0
  - - Input: (1.0, 0.0, 0.0) (0.0, 10.0, 0.0)
  - - Expected Output: 0.0
  - - Acutal Output:   0.0
  - -----------------------------------------------------------------
  - -----------------------------------------------------------------
  - - Function: vecInnerProd
  - - Test Case Number: 1
  - - Input: (1.0, 2.0, 3.0) (2.0, 4.0, 6.0)
  - - Expected Output: 28.0
  - - Acutal Output:   28.0
  - -----------------------------------------------------------------
  - -----------------------------------------------------------------
  - - Function: vecInnerProd
  - - Test Case Number: 2
  - - Input: (10.0, -10.0, 10.0) (-10.0, 10.0, -10.0)
  - - Expected Output: -300.0
  - - Acutal Output:   -300.0
  - -----------------------------------------------------------------
  - Tests for: vecDistBtwn, vecDistList, vecFindMin, vecFindMax
  - -----------------------------------------------------------------
  - - Function: vecDistBtwn
  - - Test Case Number: 0
  - - Input: (1.0, 2.0, 3.0) (-34.0, 12.0, 11.0)
  - - Expected Output: 37.26928031
  - - Acutal Output:   37.26929030716845
  - - Again, miniscule discrepancy caused by floating point error.
  - -----------------------------------------------------------------
  - -----------------------------------------------------------------
  - - Function: vecDistList
  - - Test Case Number: 0
  - - Input: (1.0, 0.0, 0.0) [ (1.0,2.0,3.0), (2.0,3.0,4.0), (4.0,3.0,5.0)]
  - - Expected Output: [3.60555, 5.09902, 6.55744]
  - - Acutal Output:   [3.60555, 5.09902, 6.55744]
  - - Floating point numbers have been rounded for simplicity.
  - -----------------------------------------------------------------
  - -----------------------------------------------------------------
  - - Function: vecFindMin
  - - Test Case Number: 0
  - - Input: (1.0, 0.0, 0.0) [ (1.0,2.0,3.0), (2.0,3.0,4.0), (4.0,3.0,5.0)]
  - - Expected Output: Just 0
  - - Acutal Output: Just 0
  - -----------------------------------------------------------------
  - -----------------------------------------------------------------
  - - Function: vecFindMax
  - - Test Case Number: 0
  - - Input: (1.0, 0.0, 0.0) [ (1.0,2.0,3.0), (2.0,3.0,4.0), (4.0,3.0,5.0)]
  - - Expected Output: Just 2
  - - Acutal Output: Just 2
  - -----------------------------------------------------------------
  - Tests for: vecF
  - -----------------------------------------------------------------
  - - Function: vecF
  - - Test Case Number: 0
  - - Input: (1.0, 2.0, 3.0) []  <- Empty vector list
  - - Expected Output: [(0.0, 0.0, 0.0),(0.0, 0.0, 0.0)]
  - - Acutal Output: [(0.0, 0.0, 0.0),(0.0, 0.0, 0.0)]
  - -----------------------------------------------------------------
  -
  - -----------------------------------------------------------------
  - - Function: vecF
  - - Test Case Number: 1
  - - Input: (1.0, 0.0, 0.0) [ (1.0,2.0,3.0), (2.0,3.0,4.0), (4.0,3.0,5.0)]
  - - Expected Output: [ (1.0,2.0,3.0), (4.0,3.0,5.0)]
  - - Acutal Output:   [ (1.0,2.0,3.0), (4.0,3.0,5.0)]
  - -----------------------------------------------------------------
  -
  - -----------------------------------------------------------------
  - - Function: vecF
  - - Test Case Number: 2
  - - Input: (2.0,3.0,4.0) [(1.0,2.0,3.0), (2.0,3.0,4.0), (4.0,3.0,5.0)]
  - - Expected Output:  [(2.0,3.0,4.0), (4.0,3.0,5.0)]
  - - Acutal Output:    [(2.0,3.0,4.0), (4.0,3.0,5.0)]
  - -----------------------------------------------------------------
  -
  - -----------------------------------------------------------------
  - - Function:
  - - Test Case Number:
  - - Input:
  - - Expected Output:
  - - Acutal Output:
  - -----------------------------------------------------------------
  -
  -}
