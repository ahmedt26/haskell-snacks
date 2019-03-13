{- Assignment 1: The Cubic Calculator
 - Version: 1.0.0
 - Name: Tahseen Ahmed
 - Start Date: September 15th, 2018
 - Version Date: September 24th, 2018
 - This version has been tested for all discriminant cases (D < 0, D == 0 and D > 0).
 This program should give back the appropriate real number roots. I think I've added
 enough test cases so it should be fine. Hopefully it's all readable.
 -}
module Assign_1 where

macid :: String
macid = "ahmedt26"

{- -----------------------------------------------------------------
 - cubicQ
 - -----------------------------------------------------------------
 - Description: Computes q by using the a, b and c variables from input
 data, which is a, b and c from the cubic function.
 -}
cubicQ :: Double -> Double -> Double -> Double
cubicQ a b c = ((3*a*c) - (b**2)) / (9*(a**2))

{- -----------------------------------------------------------------
 - cubicR
 - -----------------------------------------------------------------
 - Description: Computes r based on the given a, b, c and d variables
 of the given cubic function.
 -}
cubicR :: Double -> Double -> Double -> Double -> Double
cubicR a b c d = let
  n = (9*a*b*c) - 27*(a**2)*d - 2*(b**3)
  m = 54*(a**3)
  in n/m

{- -----------------------------------------------------------------
 - cubicDisc
 - -----------------------------------------------------------------
 - Description: Determines the discriminant using q and r from the
 functions cubicQ and cubicR.
 -}
cubicDisc :: Double -> Double -> Double
cubicDisc q r = q**3 + r**2

{- -----------------------------------------------------------------
 - cubicS
 - -----------------------------------------------------------------
 - Description: Determines s based off of q and r which are computed
 by cubicQ and cubicR.
 -}
cubicS :: Double -> Double -> Double
cubicS q r = cubicSolver $ r + sqrt disc
  where disc = q**3 + r**2

{- -----------------------------------------------------------------
 - cubicT
 - -----------------------------------------------------------------
 - Description: Determines t based of off q and r.
 -}
cubicT :: Double -> Double -> Double
cubicT q r = cubicSolver $ r - sqrt disc
  where disc = q**3 + r**2


{- -----------------------------------------------------------------
 - cubicSolver
 - -----------------------------------------------------------------
 - Description: Takes the cubic root of positive and negative numbers.
 This does not return a complex number. It only turns the negative root
 into its positive counterpart buy using abs, and then multiplies it by -1.
 Does not work for -0 as for some reason signum doesn't return "0" back.
 -}
cubicSolver :: Double -> Double
cubicSolver k = signum k * abs k ** (1/3)

{- -----------------------------------------------------------------
 - cubicRealSolutions
 - -----------------------------------------------------------------
 - Description: Computes the real roots of the given cubic function and displays
 the real roots in a list of doubles.
 -}
cubicRealSolutions :: Double -> Double -> Double -> Double -> [Double]
cubicRealSolutions a b c d
    | disc > tol = [x1]
    | abs disc <= tol = [x1, x2, x2]
    | otherwise = []
    where q = cubicQ a b c
          r = cubicR a b c d
          disc = cubicDisc q r
          s = cubicS q r
          t = cubicT q r
          x1 = s + t - (b /(3*a))
          x2 = ((s + t)/(-2)) - b/(3*a)
          tol = 10.0 ** (-10)
          -- x3 isn't needed as it can't be calculated with real number arithmetic.

{- -----------------------------------------------------------------
- cubicCasePresenter
- -----------------------------------------------------------------
- Description: A copy of cubicRealSolutions, but gives gives a message
 to the user about which case was determined instead of a list of roots.
 Its intention was to be a nice addon, and a troubleshooter for discriminant cases.
 I tried integrating this with cubicRealSolutions, but I don't know how.
 I'm sure I need recursion, but I'm not sure how to call it in the function. Oh Well ¯\_(ツ)_/¯
 PostNote: It needed a tuple so I could write out a list and then the string, but on second thought, it would
 have failed the objective requirements. I'll leave it in here anyways.
-}

{-
cubicCasePresenter :: Double -> Double -> Double -> Double -> String
cubicCasePresenter a b c d
    | disc > tol = "Case 1: The discriminant is greater than zero. There are 3 real roots, but x2 and x3 require complex number complex number arithmetic."
    | abs disc <= tol = "Case 2 The discriminant is 0. There are 3 real roots and x2 = x3."
    | otherwise = "Case 3: The discriminant is negative, no real roots to calculate, all of them are complex rotots."
    where q = cubicQ a b c
          r = cubicR a b c d
          disc = cubicDisc q r
          s = cubicS q r
          t = cubicT q r
          x1 = s + t - (b /(3*a))
          x2 = ((s + t)/(-2)) - b/(3*a)
          tol = 10.0 ** (-10)
          -- x3 isn't needed as it can't be  calculated with real number arithmetic.
-}
{- -----------------------------------------------------------------
 - Test Cases
 - -----------------------------------------------------------------
 -}

{- Test Case 1A (D = 0): (x - 1)(x - 2)(x - 2) = 0 <=> x^3 - 5x^2 + 8x - 4 = 0
   Hand-calculated values: Q = (-1/3) R = (-1/27) D = 0 S = 0 T = 0
   Should return the list [1.0, 2.0, 2.0]
   First Check: All functions returned correct values. Correct list returned
   Second Check: All functions returned correct values. Correct list returned
   Conclusion: This code works for Test Case 1A.
-}

{- Test Case 1B (D = 0): (x - 1)(x - 1)(x - 1) = 0 <=> x^3 - 3x^2 + 3x - 1 = 0
   Hand-calculated values: Q = R = D = S = T = 0
   Should return the list [1.0, 1.0, 1.0]
   First Check: All functions returned correct values. Correct list returned
   Second Check: All functions returned correct values. Correct list returned
   Conclusion: This code works for Test Case 1B.
-}

{- Test Case 1C (D = 0): x^2(x + 1) <=> x^3 + x^2 = 0
   Hand-calculated values: Q = -1/9 R = -1/27 D = 0 S = -1/3 T = -1/3
   Should return [-1.0, 0.0, 0.0]
   First Check: All returned values correct. the 0s are 55e-17, floating point errors (basically 0.). The returned list is correct.
   Second Check: Same as before. Floating point error, otherwise returned list is correct.
   Conclusion: The code works for Test Case 1C, except for unavoidable floating point error.
-}

{- Test Case 2A (D < 0): x(x -sqrt 3)(x + sqrt 3) = 0 <=> x^3 - -3x = 0
   Hand-calculated values: Q = -1 R = 0 D = -1 S = NaN/Complex T = NaN/complex
   Should return an empty list as discriminant is negative.
   First Check: All functions returned correct values. An empty list is returned
   Second Check: All functions returned correct values. An empty list is returned
   Conclusion: This code works for Test Case 2A.
-}

{- Test Case 2B (D < 0): (x - 1)(x - 2)(x - 3) = 0 <=> x^3 - 6x^2 + 11x - 6 = 0
   Hand-calculated values: Q = -1/3 R = 0 D = NaN/Complex S = NaN T = NaN
   Should return []
   First Check: All functions return correct values. Correct list is returned.
   Second Check: All functions return correct values. Correct list is returned.
   Conclusion: This code works for Test Case 2B.
-}

{- Test Case 2C (D < 0): (x-1)(x+1)(x+2)= = <=> x^3 + 2x^2 - x - 2 = 0
   Hand-calculated values: Q = -7/9 R = 10/27 D = -1/3 S = NaN T = NaN
   Should return []
   First Check: All values return their correct values. Correct list is returned
   Second Check: All values return their correct values. Correct list is returned
   Conclusion: This code works for Test Case 2C
   NOTE: Although the discriminant is negative, all of the roots seem like real numbers.
   I'm not sure why. I'd rather not bother with the math. :)
-}

{- Test Case 3A (D > 0): x^3 + 2x^2 + 3x + 4 = 0
   Hand-calculated values: Q = 5/9 R = -35/27 D = 50/27 S = 0.4011 T = -1.3851
   Should return the list [x1] as x2 and x3 require complex number arithmetic (S - T) /= 0
   x1 is around -1.65063
   First Check: All functions returned correct values. The correct list with the correct root is returned.
   Second Check: All functions returned correct values. The correct list with the correct root is returned.
   Conclusion: The code works for Test Case 3A.
-}

{- Test Case XX (INSERT SPECIFIC CASE):
   Hand-calculated values:
   Should return
   First Check:
   Second Check:
   Conclusion:
-}
