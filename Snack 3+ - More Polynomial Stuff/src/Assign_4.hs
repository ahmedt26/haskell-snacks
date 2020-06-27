{- Assignment 4 - Polynomial Party: The Sequel
 - Name: Tahseen Ahmed
 - Date: 17/11/2018 -> 18/11/2018
 -}
module Assign_4 where

import           Data.List
import           Data.Maybe
import           Test.QuickCheck

macid :: String
macid = "ahmedt26"

data Poly a = X
            | Coef a
            | Sum (Poly a) (Poly a)
            | Prod (Poly a) (Poly a)
            deriving Show

newtype PolyList a = PolyList [a] deriving Show

testfile :: FilePath
testfile = "../1JC3-Assign4/test/testFile.txt" -- empty
testfile2 = "../1JC3-Assign4/test/testFile2.txt" -- 0,1,2,3,4,5,6,7,8,9,10
testfile3 = "../1JC3-Assign4/test/testFile3.txt" -- 88,12,0,0,12,0,0

{- -----------------------------------------------------------------
- polyn
- ------------------------------------------------------------------
- A set of list polynomials to make testing easier.
-}
listPolyn :: Num a => PolyList a
listPolyn = PolyList []
listPoly1 = PolyList [1]
listPoly2 = PolyList [1,2,3,4,5]
listPoly3 = PolyList [2,3,1,5,0,1,-2,3,4,0,0] -- Unsimplified poly for testing.
listPoly4 = PolyList [1,4,8,6]
listPoly5 = PolyList [2,8,7,0,2]

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

{- -----------------------------------------------------------------
 - getPolyList
 - -----------------------------------------------------------------
 - Description: Gets coefficients from a file and makes a polyList with them.
 Each line in the file holds a coefficient and they are in order of ascending degree.
 The function reads them and turns it into a PolyList a data type. polyListSimplify
 removes excess zeros if there are any.
 -}
getPolyList :: FilePath -> IO (PolyList Integer)
getPolyList file = do
  textfile <- readFile file
  let
    stringofints = lines textfile
    listofcoefs   = map read stringofints :: [Integer]
    in
    if null stringofints then return (PolyList [0]) -- This doesn't work I keep getting just PolyList without square brackets.
    else
      return  (polyListSimplify $ PolyList listofcoefs)

{- -----------------------------------------------------------------
 - takeList
 - -----------------------------------------------------------------
 - Description: Extracts the list of coefficients from a PolyList data type.
 Used for doing some conditional statements.
 -}
takeList :: (Num a, Eq a) => PolyList a -> [a]
takeList (PolyList xs) = xs

{- -----------------------------------------------------------------
 - polyListSimplify
 - -----------------------------------------------------------------
 - Description: Simplifies the given polynomial list. Essentially, just shaves
 off excess zeros from the right side of the list.
 -}
polyListSimplify :: (Num a, Eq a) => PolyList a -> PolyList a
polyListSimplify (PolyList [])  = PolyList []
polyListSimplify (PolyList [0]) = PolyList []
polyListSimplify (PolyList [x]) = PolyList [x]
polyListSimplify (PolyList xs)
    | last xs /= 0 = PolyList xs -- Leaves non-zeros alone, shaves off zeros from the right.
    | otherwise = polyListSimplify (PolyList (init xs))

{- -----------------------------------------------------------------
 - polyListValue
 - -----------------------------------------------------------------
 - Description: Plugs n into the polynomial and returns its output, or value.
 This function uses Horner's method, as mentioned in the assignment paper.
 -}
polyListValue :: Num a => PolyList a -> a -> a
polyListValue (PolyList []) n     = 0
polyListValue (PolyList (x:xs)) n = x + n * polyListValue (PolyList xs) n

{- -----------------------------------------------------------------
 - polyListDegree
 - -----------------------------------------------------------------
 - Description: Compues the degree of the polynomial.
 -}
polyListDegree :: (Num a, Eq a) => PolyList a -> Integer
-- The undefined case for the zero polynomial is -1 because
-- I'd rather make it show something than give an error message.
polyListDegree (PolyList []) = -1
polyListDegree polyList = toInteger $ length xs - 1
    where xs = takeList $ polyListSimplify polyList

{- -----------------------------------------------------------------
 - polyListDeriv
 - -----------------------------------------------------------------
 - Description: Computes the derivative of the given polynomial.
 It applies the power rule to the entire list: it multiplies every element in
 the list by its degree (its element index) and then tails the list since its one less
 power. The method is ax^n => anx^(n-1)
 -}
polyListDeriv :: (Num a, Eq a) => PolyList a -> PolyList a
polyListDeriv (PolyList [])  = PolyList []
polyListDeriv (PolyList [x]) = PolyList []
polyListDeriv (PolyList xs)  = polyListSimplify $ PolyList $ tail $ zipWith (*) infiniIndex xs

{- -----------------------------------------------------------------
 - infiniIndex
 - -----------------------------------------------------------------
 - Description: An infinite list acting as the list of element indexes.
 Its use is to apply the part of the power rule where you multiply a by n in anx^n-1
 It's basically [0..] but since polyListDeriv doesn't use the Enum class I can't use the ..
 operator, so this is just a detour around that issue.
 -}
infiniIndex :: Num a => [a]
infiniIndex = 0 : map (+1) infiniIndex

{- -----------------------------------------------------------------
 - polyListSum
 - -----------------------------------------------------------------
 - Description: Adds two polynomials together.
 -}
polyListSum :: (Num a, Eq a) => PolyList a -> PolyList a -> PolyList a
polyListSum (PolyList []) q1 =  q1
polyListSum p1 (PolyList []) =  p1
polyListSum (PolyList xs)  (PolyList ys)
    | length xs < length ys = polyListSimplify $ PolyList $ zipWith (+) (xs ++ repeat 0) ys
    | length xs > length ys = polyListSimplify $ PolyList $ zipWith (+) xs (ys ++ repeat 0)
    {- Since zipWith stops at the shortest list, if you make the shorter polynomial
    infinitely long and retry the computation, zipWith will now add the two lists
    correctly without cutting out parts of the longer one. Also, since Haskell uses
    lazy evaluation, you won't have an infinite list of zeros since zipWith will now
    end at the former-longer, now-shorter polynomial. This isn't true if the given
    polynomial has excess zeros. My simplify function fixes that at th end of the computation.
     -}
    | otherwise = polyListSimplify $ PolyList (zipWith (+) xs ys)

{- -----------------------------------------------------------------
 - polyListProd
 - -----------------------------------------------------------------
 - Description: Multiplies two polynomials together. It multiplies one polynomial
 by each element in the other to get a list of polynomials (accounting for increasing
 power of each term). Then the function adds all of those intermediate polynomials together.
 -}
polyListProd :: (Num a, Eq a) => PolyList a -> PolyList a -> PolyList a
polyListProd (PolyList []) q1             = PolyList []
polyListProd p1 (PolyList [])             = PolyList []
polyListProd (PolyList [x]) (PolyList ys) = polyListSimplify $ PolyList $ map (\n -> x * n) ys
polyListProd (PolyList xs) (PolyList [y]) = polyListSimplify $ PolyList $ map (\n -> y * n) xs
polyListProd (PolyList xs) (PolyList ys)  = polyListSimplify $ polyListSum firstprod nextprod
  where
    prod = map (* head xs) ys
    firstprod = PolyList prod
    nextprod = polyListProd (PolyList (tail xs)) (PolyList (0:ys))

{- -----------------------------------------------------------------
- polyValue
- -----------------------------------------------------------------
- Description: Plugs n into the polynomial and returns a value.
Taken from Assignment 3.
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
power of x in the polynomial. These functions don't simplify, and won't
account for cases where the polynomial simplifies into nothing, or some lower degree.
Taken from Assignment 3.
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
 - polyListToPoly
 - -----------------------------------------------------------------
 - Description: Converts a PolyList to a Poly.
 -}
polyListToPoly :: Num a => PolyList a -> Poly a
polyListToPoly (PolyList [])     = Coef 0
polyListToPoly (PolyList [x])    = Coef x
polyListToPoly (PolyList (x:xs)) = Sum (Coef x) (Prod X $ polyListToPoly (PolyList xs))
-- The case above works kind of like Horner's method. It spits out an unsimplified Poly though.

{- -----------------------------------------------------------------
 - polyToPolyList
 - -----------------------------------------------------------------
 - Description: Converts a Poly to a PolyList.
 -}
polyToPolyList :: (Num a, Eq a) => Poly a -> PolyList a
polyToPolyList (Coef 0)                = PolyList []
polyToPolyList (Coef x)                = PolyList [x]
polyToPolyList (Sum (Coef x) (Coef y)) = PolyList [x+y]
polyToPolyList X                       = PolyList [0,1]
polyToPolyList (Prod (Coef x) X)       = PolyList [0,x]
polyToPolyList (Prod X (Coef x))       = PolyList [0,x]
polyToPolyList (Sum X X)               = PolyList [0,2]
polyToPolyList (Prod X X)              = PolyList [0,0,1]
polyToPolyList (Sum a b)               = polyListSum (polyToPolyList a) (polyToPolyList b)
polyToPolyList (Prod a b)              = polyListProd (polyToPolyList a) (polyToPolyList b)

{- -----------------------------------------------------------------
-                            Test Cases
- -----------------------------------------------------------------
- Function: polyListValue
- -----------------------------------------------------------------
- - Function: polyListValue
- - Test Case Number: 0
- - Input: (PolyList []) 121212121
- - Expected Output: 0
- - Acutal Output:   0
- -----------------------------------------------------------------
- -----------------------------------------------------------------
- - Function: polyListValue
- - Test Case Number: 1
- - Input: listPoly2 10   (listPolyn others are listed near the top of program.)
- - Expected Output: 54321
- - Acutal Output:   54321
- -----------------------------------------------------------------
- -----------------------------------------------------------------
- - Function: polyListValue
- - Test Case Number: 2
- - Input: listPoly3 -2
- - Expected Output: 440
- - Acutal Output:   440
- -----------------------------------------------------------------
- Function: polyListDegree
- -----------------------------------------------------------------
- - Function: polyListDegree
- - Test Case Number: 0
- - Input: PolyList []
- - Expected Output: -1
- - Acutal Output:   -1
- -----------------------------------------------------------------
- -----------------------------------------------------------------
- - Function: polyListDegree
- - Test Case Number: 1
- - Input: PolyList [1]
- - Expected Output: 0
- - Acutal Output:   0
- -----------------------------------------------------------------
- -----------------------------------------------------------------
- - Function: polyListDegree
- - Test Case Number: 2
- - Input: listPoly3
- - Expected Output: 8
- - Acutal Output:   8
- -----------------------------------------------------------------
- Function: polyListDeriv
- -----------------------------------------------------------------
- - Function: polyListDeriv
- - Test Case Number: 0
- - Input: PolyList []
- - Expected Output: PolyList []
- - Acutal Output:   PolyList []
- -----------------------------------------------------------------
- -----------------------------------------------------------------
- - Function: polyListDeriv
- - Test Case Number: 1
- - Input: PolyList [-1273982173982179831]
- - Expected Output: PolyList []
- - Acutal Output:   PolyList []
- -----------------------------------------------------------------
- -----------------------------------------------------------------
- - Function: polyListDeriv
- - Test Case Number: 2
- - Input: listPoly3
- - Expected Output: PolyList [3,2,15,0,5,-12,21,32]
- - Acutal Output:   PolyList [3,2,15,0,5,-12,21,32]
- -----------------------------------------------------------------
- Function: polyListSum
- -----------------------------------------------------------------
- - Function: polyListSum
- - Test Case Number: 0
- - Input: listPolyn listPolyn
- - Expected Output: PolyList []
- - Acutal Output:   PolyList []
- -----------------------------------------------------------------
- -----------------------------------------------------------------
- - Function: polyListSum
- - Test Case Number: 1
- - Input: listPoly1 listPolyn
- - Expected Output: PolyList [1]
- - Acutal Output:   PolyList [1]
- -----------------------------------------------------------------
- -----------------------------------------------------------------
- - Function: polyListSum
- - Test Case Number: 2
- - Input: listPoly3 listPoly5
- - Expected Output: PolyList [4,11,8,5,2,1,-2,3,4]
- - Acutal Output:   PolyList [4,11,8,5,2,1,-2,3,4]
- -----------------------------------------------------------------
- Function: polyListProd
- -----------------------------------------------------------------
- - Function: polyListProd
- - Test Case Number: 0
- - Input: listPolyn listPolyn
- - Expected Output: PolyList []
- - Acutal Output:   PolyList []
- -----------------------------------------------------------------
- -----------------------------------------------------------------
- - Function: polyListProd
- - Test Case Number: 1
- - Input: listPoly3 listPolyn
- - Expected Output: Polylist []
- - Acutal Output:   PolyList []
- -----------------------------------------------------------------
- -----------------------------------------------------------------
- - Function: polyListProd
- - Test Case Number: 2
- - Input: listPoly4 listPoly5
- - Expected Output: PolyList [2,16,55,104,106,50,16,12]
- - Acutal Output:   PolyList [2,16,55,104,106,50,16,12]
- -----------------------------------------------------------------
- Function: polyListToPoly
- -----------------------------------------------------------------
- - Function: polyListToPoly
- - Test Case Number: 0
- - Input: PolyList []
- - Expected Output: Coef 0
- - Acutal Output:   Coef 0
- -----------------------------------------------------------------
- -----------------------------------------------------------------
- - Function: polyListToPoly
- - Test Case Number: 1
- - Input: PolyList [1337]
- - Expected Output: Coef 1337
- - Acutal Output:   Coef 1337
- -----------------------------------------------------------------
- -----------------------------------------------------------------
- - Function: polyListToPoly
- - Test Case Number: 2
- - Input: PolyList [0,1,2,3]
- - Expected Output: A poly equivalent to 3x^3 + 2x^2 + x
- - Acutal Output: Sum (Coef 0) (Prod X (Sum (Coef 1)
(Prod X (Sum (Coef 2) (Prod X (Coef 3)))))) => 3x^3 + 2x^2 + x
- - To check if correct: polyListValue (PolyList [0,1,2,3] 1) should equal polyValue of the Output
- - Check Output: polyListValue of polyList at 2 is 34, polyValue of translated poly is 34
- -----------------------------------------------------------------
- Function: polyToPolyList
- -----------------------------------------------------------------
- - Function: polyToPolyList
- - Test Case Number: 0
- - Input: Coef 0
- - Expected Output: PolyList []
- - Acutal Output:   PolyList []
- -----------------------------------------------------------------
- -----------------------------------------------------------------
- - Function: polyToPolyList
- - Test Case Number: 1
- - Input: Coef 420691337
- - Expected Output: PolyList [420691337]
- - Acutal Output:   PolyList [420691337]
- -----------------------------------------------------------------
- -----------------------------------------------------------------
- - Function: polyToPolyList
- - Test Case Number: 2
- - Input: poly6 x^9 + 6x^8 +18x^7 + 32x^6 + 36x^5 + 24x^4 +8x^3
- - Expected Output: PolyList [0,0,0,8,24,36,32,18,6,1]
- - Acutal Output:   PolyList [0,0,0,8,24,36,32,18,6,1]
- -----------------------------------------------------------------
- Function: getPolyList
- -----------------------------------------------------------------
- - Function: getPolyList
- - Test Case Number: 0
- - Input: testfile
- - Expected Output: PolyList []
- - Acutal Output:   PolyList
- -----------------------------------------------------------------
- -----------------------------------------------------------------
- - Function: getPolyList
- - Test Case Number: 1
- - Input: tetfile2
- - Expected Output: PolyList [0,1,2,3,4,5,6,7,8,9,10]
- - Acutal Output:   PolyList [0,1,2,3,4,5,6,7,8,9,10]
- -----------------------------------------------------------------
- -----------------------------------------------------------------
- - Function: getPolyList
- - Test Case Number: 2
- - Input: testfile3
- - Expected Output: PolyList [88,12,0,0,12]
- - Acutal Output:   PolyList [88,12,0,0,12]
- -----------------------------------------------------------------
-
- -----------------------------------------------------------------
- - Function:
- - Test Case Number:
- - Input:
- - Expected Output:
- - Acutal Output:
- -----------------------------------------------------------------
-}
