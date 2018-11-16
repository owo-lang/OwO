module OwO.Util.List where

-- | Checks if all the elements in the list are equal. Assumes that
-- the 'Eq' instance stands for an equivalence relation.
allEqual :: Eq a => [a] -> Bool
allEqual []       = True
allEqual (x : xs) = all (== x) xs
