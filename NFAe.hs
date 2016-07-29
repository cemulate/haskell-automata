{-# LANGUAGE GADTs, GADTSyntax #-}

module NFAe where

import Control.Monad
import Data.Set.Monad as S

import EpsilonUtil

data NFAeDef s a where
    NFAeDef :: Ord s =>
              { states :: Set s
              , alphabet :: Set a
              , trans :: s -> Maybe a -> Set s -- trans s Nothing represents the e-moves from state s
              , start :: s
              , final :: Set s
              } -> NFAeDef s a

-- Given set of states, calculate the epsilon-closure (iteratively take all e-moves until the result stops changing)
eClosure :: (Ord s) => NFAeDef s a -> Set s -> Set s
eClosure d init = iterateUntilStable length init eFlood where
    eFlood ss = union ss (ss >>= \s -> (trans d) s Nothing) -- Union the set with all the e-moves from every state in the set

runNFAe :: NFAeDef s a -> [a] -> Bool
runNFAe d@(NFAeDef _ _ trans start final) input = any (`member` final) $ do
    initStart <- eClosure d (singleton start)        -- Before anything, take the e-closure from the start state
    foldM nTrans initStart input                     -- And now proceed with the fold
    where
        nTrans s a = eClosure d (trans s (Just a))   -- Each time, transition and then take the e-closure

-- Tests

-- Accepts an strings with an even number of 0's or 1's
test :: NFAeDef Char Char
test = NFAeDef (fromList "01234") (fromList "01") trans '0' (fromList ['1', '3']) where
    trans '0' Nothing    = fromList ['1', '3']
    trans '1' (Just '0') = singleton '2'
    trans '1' (Just '1') = singleton '1'
    trans '2' (Just '0') = singleton '1'
    trans '2' (Just '1') = singleton '2'
    trans '3' (Just '0') = singleton '3'
    trans '3' (Just '1') = singleton '4'
    trans '4' (Just '0') = singleton '4'
    trans '4' (Just '1') = singleton '3'
    trans _   _          = empty
