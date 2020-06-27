{- Assignment 5 Test Section: The Proving Grounds
- Name: Tahseen Ahmed
- Date: 29/11/2018 -> 02/12/2018
-}

import           Assign_5

import           Test.QuickCheck

main :: IO ()
main = do print "Starting Tests..."
          print "===================================="
          print "Performing definiteIntegral Prop"
          quickCheck definiteIntegralProp
          print "===================================="
          print "Performing funH Prop"
          quickCheck funHProp
          print "===================================="
          print "Performing funK Prop"
          quickCheck funKProp

definiteIntegralProp :: Double -> Double -> Integer -> Bool
definiteIntegralProp a b n
  | n < 1 = True -- Sanity check.
  | otherwise = definiteIntegral a b hx n - definiteIntegral a b fx n - definiteIntegral a b gx n < tol
      where
        hx x = fx x + gx x
        fx x = x
        gx x = 1.0
        tol = 1e-6


funHProp :: Integer -> Bool
funHProp n
  | n <= 0 = True -- Sanity check for undefined section.
  | n == 1 = funH n == 0.0
  | otherwise = 1 - lfx - funH n < tol
  where
    integralfx = definiteIntegral 0 1 (^ n) 100000
    integralgx = definiteIntegral 0 1 ( ** ( 1.0 / fromIntegral n)) 100000
    -- Leftover areas from 0 to 1 where y < 1.
    lfx = 1.0 - integralfx
    tol = 1e-6

funKProp :: Double -> Bool
funKProp a
  | a <= 0 = True -- Undefined range for a, sanity check.
  | a == 1.0 = funK a == 2.0
  | otherwise = abs (funK (1 / a) - funK a) <= tol
      where
        tol = 1e-6

{- ----------------------------------------------------------------------------
                              Property Testing
-------------------------------------------------------------------------------
Function: definiteIntegral
Property: If h(x) = f(x) + g(x), the integral of h(x) is the same as adding the
integrals of f(x) and g(x)
Actual Test Result: Pass
-------------------------------------------------------------------------------
Function: funH
Property: Imagine a square from 0 <= x <= 1 and 0 <= y <= 1.
The area of concern is this square as the point of intersection is always (1,1).
The leftover of the x^n function is 1 minus its area. If you take the square
(value of 1) and subtract the value of this leftover plus funH at n, you get 0.
Actual Test Result: Pass
-------------------------------------------------------------------------------
Function: funK
Property: The area of a^x and a^(1/x) is the same for an interval from a to b.
Actual Test Result: Pass
-------------------------------------------------------------------------------
-}
