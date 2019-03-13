{- Assignment 3 Extra Credit - The After Poly Party
 - Name: Tahseen Ahmed
 - Date: 10/29/1028 - 04/11/2018
 - v1.0.2
 -}

module Assign_3_ExtraCredit where
  import Test.QuickCheck

  macid :: String
  macid = "ahmedt26"

  {- -----------------------------------------------------------------
  - Data Types Poly and PolyAlt
  - -----------------------------------------------------------------
  - The data types for this assignment: Poly and PolyAlt.
  -}
  data Poly a = X
              | Coef a
              | Sum (Poly a) (Poly a)
              | Prod (Poly a) (Poly a)
      deriving Show

  data PolyAlt a = Monomial a Integer
                 | SumAlt (PolyAlt a) (PolyAlt a)
    deriving Show

  {- -----------------------------------------------------------------
  - polyn
  - -----------------------------------------------------------------
  - A set of polynomials to make testing easier.
  -}
  polyn :: Num a => Poly a
  polyn = Coef 233 -- 233x^0 or 233
  poly1 = X -- x
  poly2 = Sum X X -- 2x
  poly3 = Prod X X -- x^2
  poly4 = Sum poly3 poly3  -- 2x^2
  poly5 = Sum (Sum poly3 poly3) poly2 -- 2x^2 + 2x
  poly6 = Sum (Prod X poly3) (Sum (Sum poly3 poly3) poly2)  -- x^3 + 2x^2 + 2x
  poly7 = Prod poly6 (Prod poly6 poly6) -- x^9 + 6x^8 +18x^7 + 32x^6 + 36x^5 + 24x^4 +8x^3
  poly8 :: Num a => Poly a
  poly8 = Sum (Prod poly4 poly4) (Coef 10) -- 4x^4 + 10
  poly9 = Prod (Coef (-1)) X --  -1x or -x
  poly10 = Sum poly9 (Prod (Coef 1) X) -- x - x = 0
  poly11 = Sum X (Sum X (Sum X (Sum X (Sum X (Sum X (Sum X ( Sum X X))))))) -- 9x

  {- -----------------------------------------------------------------
  - polyAltn
  - -----------------------------------------------------------------
  - A set of alternative polynomials to make testing easier.
  -}
  polyAltn :: Num a => PolyAlt a
  polyAltn = Monomial 15 0  -- 15x^0 or 15
  polyAlt1 = Monomial 1 1   -- x
  polyAlt2 = Monomial 5 2   -- 5x^2
  polyAlt3 = SumAlt (Monomial 3 2) (Monomial (-5) 1) -- 3x^2 - 5x
  polyAlt4 = SumAlt (SumAlt (Monomial 13 6) (SumAlt (Monomial (-32) 3) (Monomial 115 0))) (Monomial 5 0)-- 13x^6 - 32x^3 + 115 + 5
  polyAlt5 = SumAlt (Monomial (-15) 10) (Monomial 15 10) -- -15x^10 + 15x^10 = 0
  polyAlt6 = SumAlt (Monomial 1 2) (Monomial 1 0) -- x^2 + 1
  polyAlt7 = SumAlt (Monomial 2 6) (SumAlt (Monomial (-3) 3) (Monomial 1 0))-- 2x^6 - 3x^3 + 1

  {- -----------------------------------------------------------------
   - polyValue
   - -----------------------------------------------------------------
   - Description: Plugs n into the polynomial and returns the value.
   Used for polyToPolyAlt testing.
   -}
  polyValue :: Num a => Poly a -> a -> a
  polyValue (Coef a) n   = a
  polyValue X n          = n
  polyValue (Sum a b) n  = polyValue a n + polyValue b n
  polyValue (Prod a b) n = polyValue a n * polyValue b n

  {- -----------------------------------------------------------------
  - polyAltSimplify
  - ------------------------------------------------------------------
  - Simplifies the given polynomial. It collects like terms and such.
  Doesn't fully simplify polynomials, only goes through once and simplifies what
  it can easily do. Its purpose to account for terms that cancel out.
  -}
  polyAltSimplify :: (Num a, Eq a) => PolyAlt a -> PolyAlt a
  polyAltSimplify (Monomial 0 e) = Monomial 0 0
  polyAltSimplify (Monomial c 0) = Monomial c 0
  polyAltSimplify (Monomial c e) = Monomial c e
  polyAltSimplify (SumAlt (Monomial c1 e1) (Monomial c2 e2)) =
    if e1 == e2 then Monomial (c1 + c2) e1
      else SumAlt (Monomial c1 e1) (Monomial c2 e2)
  polyAltSimplify (SumAlt a b) = SumAlt (polyAltSimplify a) (polyAltSimplify b)

  {- -----------------------------------------------------------------
  - polyAltValue
  - ------------------------------------------------------------------
  - Returns the the value of the alternative polynomial when given an input.
  -}
  polyAltValue :: Num a => PolyAlt a -> a -> a
  polyAltValue (Monomial c 0) n = c
  polyAltValue (Monomial c e) n = c * (n ^ e)
  polyAltValue (SumAlt (Monomial c1 e1) (Monomial c2 e2)) n = (c1 * (n ^ e1)) + (c2 * (n ^ e2))
  polyAltValue (SumAlt (SumAlt a b) c) n = polyAltValue a n + polyAltValue b n + polyAltValue c n
  polyAltValue (SumAlt a (SumAlt b c)) n = polyAltValue a n + polyAltValue b n + polyAltValue c n
  polayAltValue (SumAlt a b) n = polyAltValue a n + polyAltValue b n

  {- -----------------------------------------------------------------
  - polyAltDegree
  - ------------------------------------------------------------------
  - Computes the degree of the given polynomial. Does some rudimentary
  simplifcation with polyAltSimplify to account for cancallation of terms.
  -}
  polyAltDegree :: (Num a, Eq a) => PolyAlt a -> Integer
  polyAltDegree (Monomial 0 e) = 0
  polyAltDegree (Monomial c e) = e
  polyAltDegree (SumAlt (Monomial c1 e1) (Monomial c2 e2))
      | e1 == e1 = polyAltDegree (Monomial (c1 + c2) e1)
      | e1 > e2 = e1
      | otherwise = e2
  polyAltDegree (SumAlt a b)
      | polyAltDegree (polyAltSimplify a) == polyAltDegree (polyAltSimplify b) = polyAltDegree (polyAltSimplify (SumAlt a b))
      | polyAltDegree (polyAltSimplify a) > polyAltDegree (polyAltSimplify b) = polyAltDegree (polyAltSimplify a)
      | otherwise = polyAltDegree (polyAltSimplify b)
  {- -----------------------------------------------------------------
  - polyAltDeriv
  - ------------------------------------------------------------------
  - Computes the derivative of the given polynomial.
  -}
  polyAltDeriv :: (Num a)=> PolyAlt a -> PolyAlt a
  polyAltDeriv (Monomial c 0) = Monomial 0 0
  polyAltDeriv (Monomial c e) = Monomial (c * fromIntegral e) (e - 1)
  polyAltDeriv (SumAlt a b) = SumAlt (polyAltDeriv a) (polyAltDeriv b)

  {- -----------------------------------------------------------------
  - polyAltProd
  - ------------------------------------------------------------------
  - Computes the product of two polynomials (4x^2 * 2x = 8x^3)
  -}
  polyAltProd :: Num a => PolyAlt a -> PolyAlt a -> PolyAlt a
  polyAltProd (Monomial c e) (Monomial c' e') = Monomial (c * c') (e + e')
  polyAltProd a (SumAlt b c) = SumAlt (polyAltProd a b) (polyAltProd a c)
  polyAltProd (SumAlt a b) c = SumAlt (polyAltProd a c) (polyAltProd b c)

  {- The linter says this case is redundant, but I don't know how.
  polyAltProd (SumAlt a b) (SumAlt c d) = SumAlt (SumAlt ac ad) (SumAlt bc bd)
      where
        ac = polyAltProd a c
        ad = polyAltProd a d
        bc = polyAltProd b c
        bd = polyAltProd b d
  -}

  {- -----------------------------------------------------------------
  - polyAltAbove and polyAltBelow
  - ------------------------------------------------------------------
  - Checks if a series of inputs of a polynomial is above or below the x-axis
  and lets you know whether it is or not via a bool representation.
  -}
  polyAltAbove :: (Num a, Eq a, Enum a, Ord a) => PolyAlt a -> Bool
  polyAltAbove p = all (> 0) (polyAltValues p [-5000..5000])

  polyAltBelow :: (Num a, Eq a, Enum a, Ord a) => PolyAlt a -> Bool
  polyAltBelow p = all (< 0) (polyAltValues p [-5000..5000])


  {- -----------------------------------------------------------------
  - polyAltNewton
  - ------------------------------------------------------------------
  - Uses Newton's method to approximate the root of a function. Give it a starting
  x0 (s or seed), a tolerance t and a polynomial p and it will chug away to give you a
  solution. It will check if a constant polynomial is on the x-axis and will return whether
  it has a solution or not. Infinity is returned when the line is on the x-axis, and -Infinity
  is returned for when there is no root.
  -}
  polyAltNewton :: (Fractional a, Ord a, Enum a) => PolyAlt a -> a -> a -> a
  polyAltNewton p s t
    | polyAltDegree p == 0 && polyAltValue p 0 == 0 =  1/0
    | polyAltDegree p == 0 && polyAltValue p 0 /= 0 = -1/0
    | polyAltValue (polyAltDeriv p ) s <= t = polyAltNewton p (s + 1/100000) t
    {- The above condition checks if the value of f'(s) = 0 because if that were
    the case, you'd have an undefined computation, so it just shifts s a bit
    and tries again.
    -}
    | polyAltDegree p `mod` 2 == 0 && (polyAltAbove p || polyAltBelow p) = -1/0
    -- Above checks if an even function is above or below the x-axis. If it is then there's no root.
    | polyAltValue p n > t = polyAltNewton p n t
    | otherwise = n
    where
      n = s - ( fs / f's)
      fs = polyAltValue p s
      f's = polyAltValue (polyAltDeriv p) s
  {- -----------------------------------------------------------------
  - polytoPolyAlt
  - ------------------------------------------------------------------
  - Converts a polynomial with the Poly type to a polynomial with the PolyAlt type.
  They are the same polynomial, just represented in different data types.
  -}
  polyToPolyAlt :: (Num a, Eq a) => Poly a -> PolyAlt a
  polyToPolyAlt (Coef c)           = Monomial c 0
  polyToPolyAlt X                  = Monomial 1 1
  polyToPolyAlt (Sum X X)          = Monomial 2 1
  polyToPolyAlt (Prod (Coef c) X)  = Monomial c 1
  polyToPolyAlt (Prod X (Coef c))  = Monomial c 1
  polyToPolyAlt (Prod X X)         = Monomial 1 2
  polyToPolyAlt (Sum (Prod X X) X) = SumAlt (Monomial 1 2) (Monomial 1 1)
  polyToPolyAlt (Sum X (Prod X X)) = SumAlt (Monomial 1 2) (Monomial 1 1)
  polyToPolyAlt (Sum (Sum a b) c)  = SumAlt (SumAlt (polyToPolyAlt a) (polyToPolyAlt b)) (polyToPolyAlt c)
  polyToPolyAlt (Sum a (Sum b c))  = SumAlt (polyToPolyAlt a) (SumAlt (polyToPolyAlt b) (polyToPolyAlt c))
  polyToPolyAlt (Sum a b)          = SumAlt (polyToPolyAlt a) (polyToPolyAlt b)
  polyToPolyAlt (Prod a b)         = polyAltProd (polyToPolyAlt a) (polyToPolyAlt b)

  {- -----------------------------------------------------------------
  - polyAltToPoly
  - ------------------------------------------------------------------
  - Converts a PolyAlt to Poly, the reverse of the above function.
  -}
  polyAltToPoly :: (Num a, Eq a) => PolyAlt a -> Poly a
  polyAltToPoly (Monomial c 0)                         = Coef c
  polyAltToPoly (Monomial 1 1)                         = X
  polyAltToPoly (Monomial c 1)                         = Prod (Coef c) X
  polyAltToPoly (Monomial 1 e)                         = Prod X (polyAltToPoly (Monomial 1 (e - 1)))
  polyAltToPoly (Monomial c e) = Prod (Coef c) (Prod X (polyAltToPoly (Monomial 1 (e - 1))))
  polyAltToPoly (SumAlt (Monomial 1 2) (Monomial 1 1)) = Sum (Prod X X) X
  polyAltToPoly (SumAlt (Monomial 1 1) (Monomial 1 2)) = Sum (Prod X X) X
  polyAltToPoly (SumAlt a b) = Sum (polyAltToPoly a) (polyAltToPoly b)

  {- -----------------------------------------------------------------
  - polyValues and polyAltValues
  - ------------------------------------------------------------------
  - Makes a long list of ys from the given xs of each polynomial.
  Give it a list of xs, I say about 1..100.
  -}
  polyValues :: Num a => Poly a ->  [a] -> [a]
  polyValues p = map (polyValue p)

  polyAltValues :: Num a => PolyAlt a ->  [a] -> [a]
  polyAltValues p = map (polyAltValue p)

  {- -----------------------------------------------------------------
  - polyToPolyAltProp and polyAltToPolyProp
  - ------------------------------------------------------------------
  - The propositions state that the translated polynomial should return
  the exact same outputs for any domain of inputs of the original polynomial.
  -}

  polyToPolyAltProp :: (Num a, Eq a, Enum a, Show a) => Poly a -> Bool
  polyToPolyAltProp p = polyValues p xs == polyAltValues (polyToPolyAlt p) xs
      where
        xs = [-100..100]

  polyAltToPolyProp :: (Num a, Eq a, Enum a, Show a) => PolyAlt a -> Bool
  polyAltToPolyProp p = polyAltValues p xs == polyValues (polyAltToPoly p) xs
      where
        xs = [-100..100]

  {- -----------------------------------------------------------------
  - Test Cases
  - Refer to polyAltn and polyn functions for the exact polynomial and its mathematical representation.
  - ------------------------------------------------------------------
  - Function: polyAltValue
  - ------------------------------------------------------------------
  - - Function: polyAltValue
  - - Test Case Number: 0
  - - Input: polyAltn 15500.3323
  - - Expected Output: 15
  - - Acutal Output: 15.0
  - ------------------------------------------------------------------
  - ------------------------------------------------------------------
  - - Function: polyAltValue
  - - Test Case Number: 1
  - - Input: polyAlt1  1
  - - Expected Output: 1
  - - Acutal Output:   1
  - ------------------------------------------------------------------
  - ------------------------------------------------------------------
  - - Function: polyAltValue
  - - Test Case Number: 2
  - - Input: polyAlt4 2
  - - Expected Output: 696
  - - Acutal Output: 696
  - ------------------------------------------------------------------
  - Function: polyAltDegree
  - ------------------------------------------------------------------
  - - Function: polyAltDegree
  - - Test Case Number: 0
  - - Input: polyAltn
  - - Expected Output: 0
  - - Acutal Output: 0
  - ------------------------------------------------------------------
  - ------------------------------------------------------------------
  - - Function: polyAltDegree
  - - Test Case Number: 1
  - - Input: polyAlt3
  - - Expected Output: 2
  - - Acutal Output: 2
  - ------------------------------------------------------------------
  - ------------------------------------------------------------------
  - - Function: polyAltDegree
  - - Test Case Number: 2
  - - Input: polyAlt5
  - - Expected Output: 0
  - - Acutal Output: 0
  - ------------------------------------------------------------------
  - Function: polyAltDeriv
  - ------------------------------------------------------------------
  - - Function: polyAltDeriv
  - - Test Case Number: 0
  - - Input: polyAltn
  - - Expected Output: Monomial 0 0
  - - Acutal Output:   Monomial 0 0
  - ------------------------------------------------------------------
  - ------------------------------------------------------------------
  - - Function: polyAltDeriv
  - - Test Case Number: 1
  - - Input: polyAlt1
  - - Expected Output: Monomial 1 0
  - - Acutal Output:   Monomial 1 0
  - ------------------------------------------------------------------
  - ------------------------------------------------------------------
  - - Function: polyAltDeriv
  - - Test Case Number: 2
  - - Input: polyAlt5
  - - Expected Output: A poly equivalent to 0
  - - Acutal Output: SumAlt (Monomial (-150) 9) (Monomial 150 9) <=> 0
  - ------------------------------------------------------------------
  - Function: polyAltProd
  - ------------------------------------------------------------------
  - - Function: polyAltProd
  - - Test Case Number: 0
  - - Input: (Monomial 5 0) (Monomial 12 0)
  - - Expected Output: Monomial 60 0
  - - Acutal Output:   Monomial 60 0
  - ------------------------------------------------------------------
  - ------------------------------------------------------------------
  - - Function: polyAltProd
  - - Test Case Number: 1
  - - Input: (Monomial 12 12) (Monomal 2 8)
  - - Expected Output: Monomial 24 20
  - - Acutal Output:   Monomial 24 20
  - ------------------------------------------------------------------
  - ------------------------------------------------------------------
  - - Function: polyAltProd
  - - Test Case Number: 2
  - - Input: polyAlt3 polyAlt4
  - - Expected Output: A poly equivalent to 39x^8 - 65x^7 - 96x^5 + 160x^4 + 360x^2 - 600x
  - - Acutal Output: SumAlt (SumAlt (SumAlt (Monomial 39 8) (Monomial (-65) 7))
  (SumAlt (SumAlt (Monomial (-96) 5) (Monomial 160 4)) (SumAlt (Monomial 345 2)
  (Monomial (-575) 1)))) (SumAlt (Monomial 15 2) (Monomial (-25) 1)) <=> 39x^8 - 65x^7 - 96x^5 + 160x^4 + 360x^2 - 600x
  - - Double Check if correct: f(0) = 0 and f(1) = -202
  - - Double Check Result: poly at 0 and 1 outputs 0 and -202 respectively.
  - ------------------------------------------------------------------
  - Function: polyAltNewton
  - - "No instance for (Fractional Integer)" error when I try and use some of the
  higher polyAltn functions. It works when I put in raw data type, though.
  - ------------------------------------------------------------------
  - - Function: polyAltNewton
  - - Test Case Number: 0
  - - Input: (Monomial 0 0) 3 1/1000000000
  - - Expected Output: Infinity
  - - Acutal Output:   Infinity
  - ------------------------------------------------------------------
  - ------------------------------------------------------------------
  - - Function: polyAltNewton
  - - Test Case Number: 1
  - - Input: (Monomial 1 0) 2 1/123456
  - - Expected Output: -Infinity
  - - Acutal Output:   -Infinity
  - ------------------------------------------------------------------
  - ------------------------------------------------------------------
  - - Function: polyAltNewton
  - - Test Case Number: 2
  - - Input: (Monomial 1 1) 3 1/100000000
  - - Expected Output: 0
  - - Acutal Output:   0.0
  - ------------------------------------------------------------------
  - ------------------------------------------------------------------
  - - Function: polyAltNewton
  - - Test Case Number: 3
  - - Input: (SumAlt (Monomial 1 2) (Monomial 1 0)) 2 1/10000
  - - Expected Output: -Infinity
  - - Acutal Output:   -Infinity
  - ------------------------------------------------------------------
  - ------------------------------------------------------------------
  - - Function: polyAltNewton
  - - Test Case Number: 4
  - - Input: polyAlt7 4 0
  - - Expected Output: 1
  - - Acutal Output:   1
  - ------------------------------------------------------------------
  - ------------------------------------------------------------------
  - - Function: polyAltNewton
  - - Test Case Number: 5
  - - Input: polyAlt7 4 1/10000000
  - - Expected Output: 1
  - - Acutal Output:   1.1384702335070433e-7
  - - I have no idea why this happens. It works with a 0 tolerance as in case number 4.
  - ------------------------------------------------------------------
  - ------------------------------------------------------------------
  - - Function: polyAltNewton
  - - Test Case Number: 6
  - - Input: polyAlt7 0 0
  - - Expected Output: 1
  - - Acutal Output:   0.9999999999999999 basically 1
  - ------------------------------------------------------------------
  - ------------------------------------------------------------------
  - - Function: polyAltNewton
  - - Test Case Number: 7
  - - Input: polyAlt7 (-1) 0
  - - Expected Output: 0.794
  - - Acutal Output:   0.8345900259010555 => Off by about 0.05
  - - Same issue happens when I change tolerance to something other than zero
  - - like in case number 5.
  - ------------------------------------------------------------------
  - Function polyToPolyAlt
  - ------------------------------------------------------------------
  - - Function: polyToPolyAlt
  - - Test Case Number: 0
  - - Input: Coef 10
  - - Expected Output: Monomial 10 0
  - - Acutal Output:   Monomial 10 0
  - ------------------------------------------------------------------
  - ------------------------------------------------------------------
  - - Function: polyToPolyAlt
  - - Test Case Number: 1
  - - Input: X
  - - Expected Output: Monomial 1 1
  - - Acutal Output:   Monomial 1 1
  - ------------------------------------------------------------------
  - ------------------------------------------------------------------
  - - Function: polyToPolyAlt
  - - Test Case Number: 2
  - - Input: (Prod poly11 poly11)
  - - Expected Output: A polyAlt equivalent to 81x^2
  - - Acutal Output: (Very large, messy polynomial)
  - - To Test Actual Output: polyToPolyAltProp (Prod poly11 poly11)
  - - Output is true. Therefore, actual output is correct, but unbelivably
  messy and unsimplified.
  - ------------------------------------------------------------------
  - Function: polyAltToPoly
  - ------------------------------------------------------------------
  - - Function: polyAltToPoly
  - - Test Case Number: 0
  - - Input: Monomial 1337 0
  - - Expected Output: Coef 1337
  - - Acutal Output:   Coef 1337
  - ------------------------------------------------------------------
  - ------------------------------------------------------------------
  - - Function: polyAltToPoly
  - - Test Case Number: 1
  - - Input: Monomial 1 1
  - - Expected Output: X
  - - Acutal Output:   X
  - ------------------------------------------------------------------
  - ------------------------------------------------------------------
  - - Function: polyAltToPoly
  - - Test Case Number: 2
  - - Input: polyAlt3
  - - Expected Output: A poly equivalent to 3x^2 -5x
  - - Acutal Output: Sum (Prod (Coef 3) (Prod X X)) (Prod (Coef (-5)) X) => 3x^2 -5x
  - ------------------------------------------------------------------
  - ------------------------------------------------------------------
  - - Function: polyAltToPoly
  - - Test Case Number: 3
  - - Input: polyAlt4
  - - Expected Output: A poly equivalent to 13x^6 -32x^3 + 155 + 5
  - - Acutal Output: Sum (Sum (Prod (Coef 13) (Prod X (Prod X (Prod X (Prod X (Prod X X))))))
  (Sum (Prod (Coef (-32)) (Prod X (Prod X X))) (Coef 115))) (Coef 5) => 3x^6 -32x^3 + 155 + 5
  - ------------------------------------------------------------------
  - Proposition Testing: polyToPolyALt and polyAltToPoly
  - Summary: Both propositions pass for every polyn and polyAltn I've created.
  Either that means my functions are good, or I made a bad proposition. We'll find out soon enough.
  - ------------------------------------------------------------------
  - - Function:
  - - Test Case Number:
  - - Input:
  - - Expected Output:
  - - Acutal Output:
  - ------------------------------------------------------------------
  -
  - ------------------------------------------------------------------
  - - Function:
  - - Test Case Number:
  - - Input:
  - - Expected Output:
  - - Acutal Output:
  - ------------------------------------------------------------------
  -
  -}
