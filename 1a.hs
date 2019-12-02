#!/usr/bin/env stack
-- stack script --resolver lts-14.16

module Main where

main :: IO ()
main = do
  input <- getContents
  print $ sum $ map (fuel . read) $ lines input

-- `div` rounds down
fuel :: Integer -> Integer
fuel = (subtract 2) . (`div` 3)
