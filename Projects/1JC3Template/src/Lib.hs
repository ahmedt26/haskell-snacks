module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "This is a function or something."

drop1 ::Int -> [a] -> [a]
drop1 _ []     = []
drop1 0 (x:xs) = x:xs
drop1 n (x:xs) = drop1 (n - 1) xs

take1 :: Int -> [a] -> [a]
take1 _ []     = []
take1 0 (x:xs) = []
take1 n (x:xs) = x : take1 (n - 1) xs

length1 :: [a] -> Int
length1 []     = 0
length1 [x]    = 1
length1 (x:xs) = length1 [x] + length1 xs
