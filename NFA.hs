{-# LANGUAGE GADTs, GADTSyntax #-}

module NFA where

import Control.Monad
import Data.Set.Monad hiding (map)

data NFADef s a where
    NFADef :: Ord s =>
              { states :: Set s
              , alphabet :: Set a
              , trans :: s -> a -> Set s 
              , start :: s
              , final :: Set s
              } -> NFADef s a

-- Use foldM in the Set monad to provide non-determinism
runNFA :: NFADef s a -> [a] -> Bool
runNFA (NFADef _ _ trans start final) = any (`member` final) . foldM trans start

-- Accepts strings that end with b
test :: NFADef Int Char
test = NFADef (fromList [0,1]) (fromList "ab") trans 0 (singleton 1) where
    trans 0 'a' = singleton 0
    trans 0 'b' = fromList [0, 1]
    trans 1 _ = empty
