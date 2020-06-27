{- Assignment 4 Tests: The Proving Grounds
 - Name: Tahseen Ahmed
 - Date: 18/11/2018
 -}

import           Assign_4
import           Assign_4_ExtraCredit

import           Criterion.Main
import           Test.QuickCheck

main :: IO ()
main = do print "Performing Sanity Test prop1:"
          quickCheck prop1
          print ""
          print "Performing test for polyListValue"
          quickCheck polyListValueProp
          print ""
          print "Performing test for polyListDegree"
          quickCheck polyListDegreeProp
          print ""
          print "Performing test for polyListDegree"
          quickCheck polyListDerivProp
          print ""
          print "Performing test for polyListSum"
          quickCheck polyListSumProp
          print ""
          print "Performing test for polyListProd"
          quickCheck polyListProdProp
          -- TODO implement real tests

prop1 :: Int -> Bool
prop1 _ = True

polyListValueProp :: [Integer] -> Integer -> Bool
polyListValueProp xs n = polyValue (polyListToPoly (PolyList xs)) n == polyListValue (PolyList xs) n

    where
      poly = PolyList xs
      negative = polyListValue poly (-n)
      positive = polyListValue poly n


polyListDegreeProp :: [Integer] -> Bool
polyListDegreeProp [] = polyListDegree (PolyList []) == -1
polyListDegreeProp xs
    | polyListValue (polyListSimplify (PolyList xs)) 1 == 0 = True -- Checks if zero polynomial.
    | otherwise = polyListDegree (polyListSimplify $ PolyList xs) == toInteger (length xs - 1)

polyListDerivProp :: [Integer] -> Bool
polyListDerivProp []   = True
polyListDerivProp [0]  = True
polyListDerivProp xs = polydegree == derivdegree + 1
    where
      polydegree = polyListDegree (PolyList xs)
      derivdegree = polyListDegree $ polyListDeriv (PolyList xs)


polyListSumProp ::  [Integer] ->  [Integer] -> Integer -> Bool
polyListSumProp xs ys n =
  polyListValue (PolyList xs) n + polyListValue (PolyList ys) n ==
    polyListValue (polyListSum (PolyList xs) (PolyList ys)) n


polyListProdProp :: [Integer] -> [Integer] -> Integer -> Bool
polyListProdProp xs ys n =
  polyListValue (PolyList xs) n * polyListValue (PolyList ys) n ==
    polyListValue (polyListProd (PolyList xs) (PolyList ys)) n

{------------------------------------------------------------------
-                  QuickCheck Property Cases:
- -----------------------------------------------------------------
Function: polyListValue
Property: The translated PolyList (to Poly) should return the same value for all
inputs.
Actual Test Result: Pass
- -----------------------------------------------------------------
- -----------------------------------------------------------------
Function: polyListDegree
Property: The degree of the function is always one less than the
length of the coefficient. An empty list (zero poly) is undefined or -1
and is a special case for this proposition.
Actual Test Result: Pass
- -----------------------------------------------------------------
- -----------------------------------------------------------------
Function: polyListDeriv
Property: The degree of the derivative is always one less than the degree of
the original polynomial. This is not the same for the zero polynomial and is
a special case for this proposition.
Actual Test Result: Pass
- -----------------------------------------------------------------
- -----------------------------------------------------------------
Function: polyListSum
Property: Let h(x) = f(x) + g(x), therefore h(n) = f(n) + g(n) where n is any number.
Actual Test Result: Pass
- -----------------------------------------------------------------
- -----------------------------------------------------------------
Function: polyListProd
Property: Let h(x) = f(x) * g(x), therefore h(n) = f(n) * g(n) where n is any number.
Actual Test Result: Pass
- -----------------------------------------------------------------
-}
