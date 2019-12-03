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
import Data.ByteString.Char8              (ByteString)
import qualified Data.ByteString.Char8    as BS
import Data.Maybe                         (catMaybes, fromMaybe)
import Data.Vector                        (Vector)
import qualified Data.Vector              as V
import GHC.Generics                       (Generic)

type Addr = Int
type RAM  = Vector Addr

data Error = Halted RAM
  deriving (Generic, NFData, Show)

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
perform :: Opcode -> RAM -> Either Error RAM
perform (Add x y dest) ram = pure $ ram V.// [(dest, x' + y')]
  where (x', y') = (ram !? x, ram !? y)
perform (Mul x y dest) ram = pure $ ram V.// [(dest, x' * y')]
  where (x', y') = (ram !? x, ram !? y)
perform Halt ram = Left $ Halted ram

-- | Construct a vector of integers from a list of bytestrings.
-- It's optimistic - if the parse fails for an element, the value is omitted.
parseProgram :: [ByteString] -> RAM
parseProgram = V.fromList . catMaybes . map (fmap fst . BS.readInt)

-- | Run a given program in the Either monad, returning either
-- the (Right) end state, or the (Left) error state.
run :: RAM -> Either Error RAM
run ram = V.foldM' eval ram offsets
  where offsets = V.fromList [n | n <- [0..V.length ram], n `mod` 4 == 0]
        eval akku offset =
          case akku `atOffset` offset of
            Just opcode -> perform opcode akku
            Nothing     -> pure akku

-- | Convenience function to run with "arguments".
runWithArgs :: Addr -> Addr -> RAM -> Either Error RAM
runWithArgs n m ram = run $ ram V.// [(1, n), (2, m)]
  
main :: IO ()
main = do
  input <- BS.getContents
  let program = parseProgram $ BS.split ',' input
  case runWithArgs 12 2 program of
    Right state         -> putStrLn $ "FINISHED " ++ show state
    Left (Halted state) -> putStrLn $ "HALTED " ++ show state    
