#!/usr/bin/env stack
-- stack script --resolver lts-14.16

module Main where

main :: IO ()
main = do
  input <- getContents
  print $ sum $ map (recfuel . read) $ lines input
  
recfuel :: Integer -> Integer
recfuel n = go n 0
  where go n total
          | fuel n <= 0 = total
          | otherwise   = let n' = fuel n in go n' (total + n')

-- `div` rounds down
fuel :: Integer -> Integer
fuel = (subtract 2) . (`div` 3)
