{-# LANGUAGE GADTs, GADTSyntax #-}

module DFA where

import Control.Monad
import Data.Set.Monad hiding (foldl, map)

data DFADef s a where
    DFADef :: Ord s =>
              { states :: Set s
              , alphabet :: Set a
              , trans :: s -> a -> s
              , start :: s
              , final :: Set s
              } -> DFADef s a

runDFA :: DFADef s a -> [a] -> Bool
runDFA (DFADef _ _ trans start final) = (`member` final) . foldl trans start

-- Tests

-- Accepts strings with an odd number of b's
test :: DFADef Int Char
test = DFADef (fromList [0, 1]) (fromList "ab") trans 0 (singleton 1) where
    trans s 'a' = s
    trans s 'b' = if (s == 0) then 1 else 0
