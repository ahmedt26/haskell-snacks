{- Assignment 3 - Polynomial Party
- Name: Tahseen Ahmed
- Date: 25/10/18 -> 28/10/2018 => 04/11/2018
- v1.3.5
-}

module Assign_3 where

macid :: String
macid = "ahmedt26"

data Poly a = X
          | Coef a
          | Sum (Poly a) (Poly a)
          | Prod (Poly a) (Poly a)
  deriving Show

{- -----------------------------------------------------------------
- polyn
- ------------------------------------------------------------------
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


{-

polySimplify is DEPRECIATED because we all found out we don't need to check for special cases.
I'm leaving this here as a mark of my effort.
Thank you Jack for finding out if we needed to do this or not, the entire CS class appreciates it greatly.
You don't need to read this bit in any way, unless you're curious.
{-------------------------------------------------------------------
- polySimplify
- ------------------------------------------------------------------
- Description: Simplifies the given polynomial. Adds like terms and such.
A prerequisite function for polyDegree and polyDeriv.
-}

polySimplify :: (Num a, Eq a) => Poly a -> Poly a
-- Side Note: This surely is much bulkier than it should be. This probably doesn't account for all the cases anyways. :(
-- Other Side Node: It is ridiculous how complex this needs to be just to simpify something like  a - b where a = b.
-- I blame it on the data type as it's not flexible (any my lack of knowledge.)
-- Cases increase in order of complexity, from simple constants, all the way to the large, compound patterns.

-- Constant Cases
polySimplify (Coef c)                   = Coef c
polySimplify (Sum (Coef c1) (Coef c2))  = Coef (c1 + c2)
polySimplify (Prod (Coef c1) (Coef c2)) = Coef (c1 * c2)

-- Coefficient Polynomial Cases
polySimplify X                                   = X
polySimplify (Prod (Coef 1) X)                   = X
polySimplify (Prod X (Coef 1))                   = X
polySimplify (Prod (Coef 1) a)                   = a
polySimplify (Prod a (Coef 1))                   = a
polySimplify (Prod (Coef 0) X)                   = Coef 0
polySimplify (Prod X (Coef 0))                   = Coef 0
polySimplify (Prod (Coef 0) a)                   = Coef 0
polySimplify (Prod a (Coef 0))                   = Coef 0
polySimplify (Prod (Coef c) X)                   = Prod (Coef c) X
polySimplify (Prod X (Coef c))                   = Prod (Coef c) X
polySimplify (Prod (Prod (Coef c2) a) (Coef c1)) = polySimplify $ Prod (Coef (c1 * c2)) a
polySimplify (Prod (Coef c1) (Prod (Coef c2) a)) = polySimplify $ Prod (Coef (c1 * c2)) a
polySimplify (Prod (Coef c) a)                   = Prod (Coef c) (polySimplify a)
polySimplify (Prod a (Coef c))                   = Prod (Coef c) (polySimplify a)

-- Sum cases
polySimplify (Sum X (Coef 0))  = X
polySimplify (Sum (Coef 0) X)  = X
polySimplify (Sum X X)         = Prod (Coef 2) X
polySimplify (Sum (Coef c) X)  = Sum X (Coef c)
polySimplify (Sum X (Coef c))  = Sum X (Coef c)
polySimplify (Sum (Prod (Coef c1) X) (Prod (Coef c2) X)) = if c1 == c2 then Coef 0 else Prod (Coef (c1 + c2)) X
polySimplify (Sum (Prod (Coef c1) a) (Prod (Coef c2) b)) =
if c1 == c2 && polyDegree a == polyDegree b && polyValueList a == polyValueList b
  then Coef 0
  else polySimplify $ Prod (Coef (c1 + c2)) X
polySimplify (Sum a (Sum b c)) = Sum (polySimplify a) (Sum (polySimplify b) (polySimplify c))
polySimplify (Sum (Sum b c) a) = Sum (polySimplify a) (Sum (polySimplify b) (polySimplify c))
polySimplify (Sum a b)         = Sum (polySimplify a) (polySimplify b)

-- Product Cases
polySimplify (Prod X X) = Prod X X
polySimplify (Prod a (Sum (Coef c1) (Coef c2))) = polySimplify (Sum (Prod (Coef c1) sa) (Prod (Coef c2) sa))
  where sa = polySimplify a
polySimplify (Prod (Sum (Coef c1) (Coef c2)) a) = polySimplify (Sum (Prod (Coef c1) sa) (Prod (Coef c2) sa))
  where sa = polySimplify a
polySimplify (Prod (Sum a b) (Sum c d)) = polySimplify (Sum (Sum (Prod sa sc) (Prod sa sd)) (Sum (Prod sb sc) (Prod sb sd)))
  where
    sa = polySimplify a
    sb = polySimplify b
    sc = polySimplify c
    sd = polySimplify d
polySimplify (Prod a b) = Prod (polySimplify a) (polySimplify b)

-}

{- -----------------------------------------------------------------
- polyValue
- -----------------------------------------------------------------
- Description: Plugs n into the polynomial and returns a value.
-}
polyValue :: Num a => Poly a -> a -> a
polyValue (Coef a) n   = a
polyValue X n          = n
polyValue (Sum a b) n  = polyValue a n + polyValue b n
polyValue (Prod a b) n = polyValue a n * polyValue b n

{- -----------------------------------------------------------------
- polyDegree
- -----------------------------------------------------------------
- Description: Determines the degree of the polynomial, or the highest
power of x in the polynomial.
These functions don't simplify, and won't account for cases where the polynomial
simplifies into nothing, or some lower degree.
-}
polyDegree :: (Num a, Eq a) => Poly a -> Integer
polyDegree (Coef c)   = 0
polyDegree X          = 1
polyDegree (Sum X X)  = 1
polyDegree (Prod X X) = 2
polyDegree (Sum a b)
    | polyDegree a > polyDegree b = polyDegree a
    | otherwise                   = polyDegree b
polyDegree (Prod a b)
    | polyDegree a == 0 || polyDegree b == 0 = 0 -- Anything times 0 is 0, so the degree of that will be just 0.
    | otherwise = polyDegree a + polyDegree b

{- -----------------------------------------------------------------
- polyDeriv
- -----------------------------------------------------------------
- Description: Computes the derivative of a given polynomial.
-}
polyDeriv :: Num a => Poly a -> Poly a
polyDeriv (Coef c)          = Coef 0
polyDeriv X                 = Coef 1
polyDeriv (Prod (Coef c) X) = Coef c
polyDeriv (Prod X (Coef c)) = Coef c
polyDeriv (Prod X X)        = Prod (Coef 2) X
polyDeriv (Prod (Coef c) a) = Prod (Coef c) (polyDeriv a)
polyDeriv (Prod a (Coef c)) = Prod (Coef c) (polyDeriv a)
polyDeriv (Sum a b)         = Sum (polyDeriv a) (polyDeriv b)
polyDeriv (Prod a b)        = Sum (Prod a' b) (Prod a b')
  where
    a' = polyDeriv a
    b' = polyDeriv b
{- -----------------------------------------------------------------
- Test Cases
- Refer to polyn functions for what they represent.
- -----------------------------------------------------------------
- Function : polySimplify (DEPRECIATED FUNCTION)
- -----------------------------------------------------------------
- - Function: polySimplify (DEPRECIATED FUNCTION)
- - Test Case Number: 0
- - Input: polyn
- - Expected Output: Coef 233
- - Acutal Output: Coef 233
- -----------------------------------------------------------------
- -----------------------------------------------------------------
- - Function: polySimplify (DEPRECIATED FUNCTION)
- - Test Case Number: 1
- - Input: poly10 =>
- - Expected Output: Prod (Coef 0) X
- - Acutal Output:   Prod (Coef 0) X
- -----------------------------------------------------------------
- -----------------------------------------------------------------
- - Function: polySimplify (DEPRECIATED FUNCTION)
- - Test Case Number: 2
- - Input: poly11
- - Expected Output: Prod (Coef 9) X would be the simplest form
- - Acutal Output: Sum X (Sum X (Sum X (Sum X (Sum X (Sum X (Sum X (Sum X X))))))) (no change)
- - Reason: I can't think of a recursive pattern to add each x to the product. :(
- -----------------------------------------------------------------
- Function: polyValue
- -----------------------------------------------------------------
- - Function: polyValue
- - Test Case Number: 0
- - Input: (Coef 50) 200
- - Expected Output: 50
- - Acutal Output: 50
- -----------------------------------------------------------------
- -----------------------------------------------------------------
- - Function: polyValue
- - Test Case Number: 1
- - Input: X 1212
- - Expected Output: 1212
- - Acutal Output: 1212
- -----------------------------------------------------------------
- -----------------------------------------------------------------
- - Function: polyValue
- - Test Case Number: 2
- - Input: poly6 5       (poly6 is x^3 + 2x^2 + 2x, refer to polyn functions)
- - Expected Output: 185
- - Acutal Output: 185
- -----------------------------------------------------------------
- -----------------------------------------------------------------
- - Function: polyValue
- - Test Case Number: 3
- - Input: poly7 (0.5)
- - Expected Output: 4.2910156
- - Acutal Output:   4.291015625
- -----------------------------------------------------------------
- Function: polyDegree
- -----------------------------------------------------------------
- - Function: polyDegree
- - Test Case Number: 0
- - Input: Coeff 41
- - Expected Output: 0
- - Acutal Output: 0
- -----------------------------------------------------------------
- -----------------------------------------------------------------
- - Function: polyDegree
- - Test Case Number: 1
- - Input: X
- - Expected Output: 1
- - Acutal Output: 1
- -----------------------------------------------------------------
- -----------------------------------------------------------------
- - Function: polyDegree
- - Test Case Number: 2
- - Input: poly6
- - Expected Output: 3
- - Acutal Output: 3
- -----------------------------------------------------------------
- -----------------------------------------------------------------
- - Function: polyDegree
- - Test Case Number: 3
- - Input: poly7
- - Expected Output: 9
- - Acutal Output: 9
- -----------------------------------------------------------------
- -----------------------------------------------------------------
- - Function: polyDegree
- - Test Case Number: 4
- - Input: poly10
- - Expected Output: 1
- - Acutal Output: 1
- - This should be 0, but these functions don't account for simplification.
- -----------------------------------------------------------------
- Function: polyDeriv
- - The expected outputs of the latter cases are given as equivalents because
- - there several forms as what each poly could be output as. The <=> operator
- - in these cases translate what inputs/outputs are mathematically.
- -----------------------------------------------------------------
- - Function: polyDeriv
- - Test Case Number: 0
- - Input: Coef 300 <=> 300x^0 or just 300
- - Expected Output: Coef 0 <=> 0
- - Acutal Output:   Coef 0 <=> 0
- -----------------------------------------------------------------
- -----------------------------------------------------------------
- - Function: polyDeriv
- - Test Case Number: 1
- - Input: Prod (Coef 300) X  <=> 300x
- - Expected Output: Coef 300 <=> 300
- - Acutal Output:   Coef 300 <=> 300
- -----------------------------------------------------------------
- -----------------------------------------------------------------
- - Function: polyDeriv
- - Test Case Number: 2
- - Input: poly5 <=> 2x^2 + 2x
- - Expected Output: A poly equivalent to 4x + 2
- - Acutal Output: Sum (Sum (Sum X X) (Sum X X)) (Sum (Coef 1) (Coef 1)) <=> 4x + 2
- -----------------------------------------------------------------
- -----------------------------------------------------------------
- - Function: polyDeriv
- - Test Case Number: 3
- - Input: poly6 <=> x^3 + 2x^2 + 2x
- - Expected Output: A poly equivalent to 3x^2 + 4x + 2
- - Acutal Output: Sum (Sum (Prod (Coef 1) (Prod X X)) (Prod X (Sum X X)))
(Sum (Sum (Sum X X) (Sum X X)) (Sum (Coef 1) (Coef 1))) <=> 3x^2 + 4x + 2
- - Check Output: polyValue (polyDeriv poly6) 12 should return 482
- - Check Result: returns 482, the actual output is correct
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
