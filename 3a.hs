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
import           Data.Function         ((&))
import           Data.Hashable
import           Data.HashSet          (HashSet)
import qualified Data.HashSet          as HS
import           Data.List             (foldl', sort)
import           Data.Maybe            (mapMaybe)
import           GHC.Generics          (Generic)

data Point = Point Integer Integer
  deriving (Generic, NFData, Eq, Hashable)

data Wire  = Wire [Movement]
  deriving (Generic, NFData)

data Movement = U Integer | L Integer | D Integer | R Integer
  deriving (Generic, NFData)

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
distance (Point x y) (Point x' y') = abs (x - x') + abs (y - y')

-- | Update a Point by applying a Movement to it.
move :: Point -> Movement -> Point
move (Point x y) (U n) = Point x $ y + n
move (Point x y) (D n) = Point x $ y - n
move (Point x y) (R n) = Point (x + n) y
move (Point x y) (L n) = Point (x - n) y

-- | Return a "path" (list of all passed points) for a Movement from a Point.
path :: Point -> Movement -> HashSet Point
path (Point x y) (U n) = HS.fromList [Point x' y' | x' <- [x], y' <- [y .. y + n]]
path (Point x y) (R n) = HS.fromList [Point x' y' | x' <- [x .. x + n], y' <- [y]]
path (Point x y) (D n) = HS.fromList [Point x' y' | x' <- [x], y' <- reverse [y - n .. y]]
path (Point x y) (L n) = HS.fromList [Point x' y' | x' <- reverse [x - n .. x], y' <- [y]]

-- | Run a Wire and return its visited locations.
runWire :: Wire -> HashSet Point
runWire (Wire w) = snd $ foldl' update (Point 0 0, HS.empty) w
  where
    update :: (Point, HashSet Point) -> Movement -> (Point, HashSet Point)
    update (point, locs) movement =
      (move point movement, HS.union (path point movement) locs)

processInput :: ByteString -> [Wire]
processInput = map (Wire . mapMaybe parseMovement . BS.split ',') . BS.lines

main :: IO ()
main = do
  wires <- processInput <$> BS.getContents
  let (w1, w2) = (wires !! 0, wires !! 1) in do
    print $ runWire w1 `HS.intersection` runWire w2
      & HS.map (distance $ Point 0 0)
      & HS.toList & filter (> 0) & sort
