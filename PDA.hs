{-# LANGUAGE GADTs, GADTSyntax, NamedFieldPuns #-}

module PDA where

import Control.Monad
import Data.Set.Monad as S

import EpsilonUtil

data PDADef s a b where
    PDADef :: (Ord s, Ord b) =>
              { states :: Set s
              , alphabet :: Set a
              , stackAlphabet :: Set b
              , trans :: s -> Maybe a -> b -> Set (s, [b])
              , start :: s
              , stackStart :: b
              , final :: Set s
              } -> PDADef s a b

-- Snapshot defined by the state and the stack
type Snapshot s b = (s, [b])

-- Converts a set of transition results of the form (newState, stringToPush)
-- and performs the push, "applying" the results to get the new Snapshots
prefixWithStack :: (Ord s, Ord b) => [b] -> Set (s, [b]) -> Set (Snapshot s b)
prefixWithStack stack actions = S.map (\(s, push) -> (s, push ++ stack)) actions

-- Given set of snapshots, calculate the epsilon-closure
eClosure :: PDADef s a b -> Set (Snapshot s b) -> Set (Snapshot s b)
eClosure PDADef{trans} init = iterateUntilStable length init eFlood where
    eFlood ss = union ss (ss >>= next)       -- union the set with all the possible e-moves from every snapshot in the set
    next (s, []) = empty
    -- Do the e-moves, and perform the stack action of each result
    next (s, (h:st)) = let results = trans s Nothing h in prefixWithStack st results

runPDAFinal :: (Ord s, Ord b) => PDADef s a b -> [a] -> Bool
runPDAFinal d@(PDADef _ _ _ trans start stackStart final) input = accept $ do
    initStart <- eClosure d $ singleton (start, [stackStart]) -- Construct the start snapshot, and take the e-closure
    foldM nTrans initStart input                              -- Now, proceed with the fold
    where
        nTrans (cur, []) _ = empty
        -- Get the results of feeding x, apply the stack action of each, and then take the e-closure
        nTrans (cur, (h:rest)) x = eClosure d $ let results = trans cur (Just x) h in prefixWithStack rest results
        accept = any (`member` final) . S.map fst

-- Tests

-- Accepts 0^n 1^n
test :: PDADef Char Char Char
test = PDADef (fromList "pqr") (fromList "01") (fromList "AZ") trans 'p' 'Z' (singleton 'r') where
    trans 'p' (Just '0') 'Z' = singleton ('p', "AZ")    -- p ----- 0; Z/AZ -----> p
    trans 'p' (Just '0') 'A' = singleton ('p', "AA")    -- p ----- 0; A/AA -----> p
    trans 'p' Nothing    'Z' = singleton ('q', "Z")     -- p ----- e; Z/Z ------> q
    trans 'p' Nothing    'A' = singleton ('q', "A")     -- p ----- e; A/A ------> q
    trans 'q' (Just '1') 'A' = singleton ('q', "")      -- q ----- 1; A/e ------> q
    trans 'q' Nothing    'Z' = singleton ('r', "Z")     -- q ----- e; Z/Z ------> r
    trans _   _           _  = empty
