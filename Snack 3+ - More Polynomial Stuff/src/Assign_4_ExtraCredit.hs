{- Assignment 4 Extra Credit
 - Name: TODO add full name
 - Date: TODO add of completion
 -}
module Assign_4_ExtraCredit where

import Criterion.Main

macid = "TODO: put your mac id here"


data Nat = Z
         | S Nat

data Digit = Zero
           | One deriving Show

data BinNat = Atom Digit
            | Compound BinNat Digit
