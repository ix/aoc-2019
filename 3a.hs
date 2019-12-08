#!/usr/bin/env stack
{- stack script --resolver lts-14.16
   --package bytestring
   --package deepseq
   --package unordered-containers
-}

{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE OverloadedLists #-}

module Main where

import           Control.DeepSeq       (NFData)
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.HashSet          as HS
import           Data.HashSet          (HashSet)
import           Data.Maybe            (mapMaybe)
import           GHC.Generics          (Generic)

type Point = (Integer, Integer)
type Wire  = [Movement]

data Movement = U Integer | L Integer | D Integer | R Integer
  deriving (Generic, NFData, Show)

-- | Attempt to parse a Movement from a ByteString,
-- returning the remainder of the ByteString too.
parseMovement' :: ByteString -> Maybe (Movement, ByteString)
parseMovement' bstr = do
  (x, xs)    <- BS.uncons bstr
  (n, rest)  <- BS.readInteger xs
  direction  <- asMovement x <*> pure n
  return (direction, rest)
  where
    asMovement 'U' = pure U
    asMovement 'R' = pure R
    asMovement 'D' = pure D
    asMovement 'L' = pure L
    asMovement _   = Nothing

-- | Like parseMovement' but discards the remainder.
parseMovement :: ByteString -> Maybe Movement
parseMovement = fmap fst . parseMovement'

-- | Two-dimensional Manhattan distance.
distance :: Point -> Point -> Integer
distance (x, y) (x', y') = abs (x - x') + abs (y - y')

-- | Update a Point by applying a Movement to it.
move :: Point -> Movement -> Point
move (x, y) (U n) = (x, y - n)
move (x, y) (D n) = (x, y + n)
move (x, y) (R n) = (x + n, y)
move (x, y) (L n) = (x - n, y)

-- | Run a Wire and return its final location.
runWire :: Wire -> Point
runWire = foldl move (0, 0)

intersections :: (Point, Point) -> (Point, Point) -> HashSet Point
intersections (s, e) (s', e') = HS.intersection lineA lineB
  where 
    lineA = HS.fromList [(x', y') | x' <- fst s `to` fst e, y' <- snd s `to` snd e]
    lineB = HS.fromList [(x', y') | x' <- fst s' `to` fst e', y' <- snd s' `to` snd e']
    a `to` b = if b > a then [a..b] else reverse [b..a]

processInput :: ByteString -> [Wire]
processInput = map (mapMaybe parseMovement . BS.split ',') . BS.lines

main :: IO ()
main = do
  input <- BS.getContents
  print $ processInput input
