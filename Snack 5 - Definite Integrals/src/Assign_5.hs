{- Assignment 5: Definitely Definite Integrals
- Name: Tahseen Ahmed
- Date: 29/11/2018 -> 02/12/2018
-}
module Assign_5 where

macid :: String
macid = "ahmedt26"

{- -----------------------------------------------------------------
- funcn
- -----------------------------------------------------------------
- A set of functions to make testing easier.
-}

funcn ::Double -> Double
funcn x = 2.0
func1 ::Double -> Double
func1 x = x ** 2
func2 ::Double -> Double
func2 x = x / 5 + 23.0
func3 ::Double -> Double
func3 x = x ** 3 + 2 * (x ** 2) + 2.0
func4 ::Double -> Double
func4 x = ( x ** 2 / 5) - (2 * x) - 5.0
func5 ::Double -> Double
func5 = sqrt

{- -----------------------------------------------------------------
- definiteIntegral
- -----------------------------------------------------------------
- Description: Computes the definite integral of a function.
-}

{-
An n value of 16166440 (my max) takes about 10.67s to compute an integral with my setup.
An n of 3 million provides reasonable speed (about 2s) and precision.
-}
definiteIntegral :: Double -> Double -> (Double -> Double) -> Integer -> Double
definiteIntegral a b g n
    {- The enumeration operator doesn't work when the start is greater than the end,
    so the below case switches the limits and makes the integral negative to fix that.
    This case is derived from one of the properties of integrals we all love and know.
    -}
    | b < a = (-1.0) * definiteIntegral b a g n
    | n < 1 = error "You can't have n be less than i in summation"
    | abs ( b - a) < tol = 0.0
  {- The case below makes a list of xs between a and b, and maps the function
  over each x to compute an area and adds all the areas together.
  -}
    | otherwise = sum [ g x | x <- [a , (a + deltax) .. b]] * deltax - (g a + g b) * deltax / 2
    where
      deltax = (b - a) / fromIntegral n
      tol = 1e-6

{- -----------------------------------------------------------------
- funH
- -----------------------------------------------------------------
- Description: Computes the area between the nth root of x and x to the n.
Where n is an integer. The function is undefined when n <= 0.
-}
funH :: Integer -> Double
funH n
    | n <= 0 = error "You can't do that, that's undefined! >:("
    | n < 1  = integralfx - integralgx -- Doesn't work for integers, but accounts decimal powers.
    | n == 1 = 0.0 -- Both functions are y = x.
    | otherwise = integralgx - integralfx
    where
      integralfx = definiteIntegral 0 1 ( ^ n) 100000
      integralgx = definiteIntegral 0 1 ( ** ( 1.0 / fromIntegral n)) 100000

{- -----------------------------------------------------------------
- funK
- -----------------------------------------------------------------
- Description: Computes the area under the graph y = a^x where a is some number.
This function is undefined when a <= 0.
-}
funK :: Double -> Double
funK a
    | a <= 0 = error "You can't do that! That's undefined! >:("
    | otherwise = definiteIntegral (-1) 1 fx 100000
        where
          fx x = a ** x

{- -----------------------------------------------------------------
- Test Cases
- ------------------------------------------------------------------
- Function: definiteIntegral
- -----------------------------------------------------------------
- - Function: definiteIntegral
- - Test Case Number: 0
- - Input: (-10) 10 funcn 1
- - Expected Output: 40.0
- - Acutal Output:   40.0
- -----------------------------------------------------------------
- -----------------------------------------------------------------
- - Function: definiteIntegral
- - Test Case Number: 1
- - Input: 2 3 func1 3000000
- - Expected Output: 19/3 or 6.3333333
- - Acutal Output:   6.333333333705924
- -----------------------------------------------------------------
- -----------------------------------------------------------------
- - Function: definiteIntegral
- - Test Case Number: 2
- - Input: (-23) 222 func4 3000000
- - Expected Output: 680234.333333333
- - Acutal Output:   680234.3333823952
- -----------------------------------------------------------------
- -----------------------------------------------------------------
- - Function: definiteIntegral
- - Test Case Number: 3
- - Input: (-23) 222 func4 (-2)
- - Expected Output: error "You can't have n be less than i in summation"
- - Acutal Output:   Error shows up in log correctly.
- -----------------------------------------------------------------
- Function: funH
- -----------------------------------------------------------------
- - Function: funH
- - Test Case Number: 0
- - Input: (-2)
- - Expected Output: "You can't do that, that's undefined! >:(" error
- - Acutal Output: Error shows up in in log correctly.
- -----------------------------------------------------------------
- -----------------------------------------------------------------
- - Function: funH
- - Test Case Number: 1
- - Input: 1
- - Expected Output: 0.0
- - Acutal Output:   0.0
- -----------------------------------------------------------------
- -----------------------------------------------------------------
- - Function: funH
- - Test Case Number: 2
- - Input: 5
- - Expected Output: 2/3 or 0.666666666
- - Acutal Output:   0.6666663169614506
- -----------------------------------------------------------------
- Function: funK
- -----------------------------------------------------------------
- - Function: funK
- - Test Case Number: 0
- - Input: 0
- - Expected Output: error "You can't do that! That's undefined! >:("
- - Acutal Output: Error correctly shows up in log.
- -----------------------------------------------------------------
- -----------------------------------------------------------------
- - Function: funK
- - Test Case Number: 1
- - Input: 2
- - Expected Output: 2.164042561 or 3/2ln(2)
- - Acutal Output:   2.1640425613699272
- -----------------------------------------------------------------
- -----------------------------------------------------------------
- - Function: funK
- - Test Case Number: 2
- - Input: 75
- - Expected Output: 17.36812348 or 5624/75ln(75)
- - Acutal Output:   17.368123487991298
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
