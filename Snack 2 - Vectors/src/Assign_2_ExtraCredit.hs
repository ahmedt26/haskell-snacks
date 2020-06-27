{- Assignment 1 Extra Credit
 - Name: TODO add full name
 - Date: TODO add of completion
 -}
module Assign_2_ExtraCredit where

macid = "TODO: put your mac id here"

newtype Vector2 a = Vector2 (a,a)
  deriving (Show,Eq)
newtype Vector3 a = Vector3 (a,a,a)
  deriving (Show,Eq)
newtype Vector4 a = Vector4 (a,a,a,a)
  deriving (Show,Eq)

class VectorSpace v where
  vecZero       :: (Num a) => v a
  vecSum        :: (Num a) => v a -> v a -> v a
  vecScalarProd :: (Num a) => a -> v a -> v a
  vecMagnitude  :: (Floating a) => v a -> a
  vecInnerProd  :: (Num a) => v a -> v a-> a
