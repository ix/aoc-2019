#!/usr/bin/env stack
{- stack script --resolver lts-14.16
   --package vector
   --package bytestring
   --package deepseq
-}

{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE OverloadedLists #-}

module Main where

import Control.DeepSeq                    (NFData)
import Control.Monad.Except
import Data.ByteString.Char8              (ByteString)
import qualified Data.ByteString.Char8    as BS
import Data.Maybe                         (catMaybes, fromMaybe)
import Data.Vector                        (Vector)
import qualified Data.Vector              as V
import GHC.Generics                       (Generic)

type Addr = Int
type RAM  = Vector Addr

data Opcode = Add  Addr Addr Addr
            | Mul  Addr Addr Addr
            | Halt
  deriving (Generic, NFData, Show)

-- | Given a vector of 4 integers, return the Opcode
-- represented therein, if valid.
listToOpcode :: RAM -> Maybe Opcode
listToOpcode [1, src1, src2, dest] = Just $ Add src1 src2 dest
listToOpcode [2, src1, src2, dest] = Just $ Mul src1 src2 dest
listToOpcode [99, _, _, _]         = Just Halt
listToOpcode xs                    = Nothing

-- | Given a vector of integers and an address
-- return the Opcode if a valid one exists at that point.
atOffset :: RAM -> Addr -> Maybe Opcode
atOffset xs ix =
  case V.take 4 $ V.drop ix xs of
    []  -> Nothing
    xs' -> listToOpcode xs'

(!?) :: RAM -> Addr -> Addr
xs !? n = fromMaybe 0 $ xs V.!? n

-- | Update a vector by performing an Opcode on it.
perform :: Opcode -> RAM -> RAM
perform (Add x y dest) ram = ram V.// [(dest, x' + y')]
  where (x', y') = (ram !? x, ram !? y)
perform (Mul x y dest) ram = ram V.// [(dest, x' * y')]
  where (x', y') = (ram !? x, ram !? y)
perform Halt ram = error ("HALTED AT STATE: " ++ show ram) -- shouldn't be partial but w/e

-- | Construct a vector of integers from a list of bytestrings.
-- It's optimistic - if the parse fails for an element, the value is omitted.
parseProgram :: [ByteString] -> RAM
parseProgram = V.fromList . catMaybes . map (fmap fst . BS.readInt)

run :: RAM -> RAM
run ram = V.foldl' eval ram offsets
  where offsets = V.fromList [n | n <- [0..V.length ram], n `mod` 4 == 0]
        eval akku offset =
          case akku `atOffset` offset of
            Just opcode -> perform opcode akku
            Nothing     -> akku

runWithArgs :: Addr -> Addr -> RAM -> RAM
runWithArgs n m ram = run $ ram V.// [(1, n), (2, m)]
  
main :: IO ()
main = do
  input <- BS.getContents
  let program = parseProgram $ BS.split ',' input
  print $ runWithArgs 12 2 program
