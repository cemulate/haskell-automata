module EpsilonUtil where

-- For a predicate, initial value, and iterator, iterate until the value of the predicate becomes identical, and return that element
iterateUntilStable :: (Eq b) => (a -> b) -> a -> (a -> a) -> a
iterateUntilStable pred init f = inner $ iterate f init where
    inner (x:y:zs)
        | (pred x) == (pred y) = y
        | otherwise            = inner (y:zs)
