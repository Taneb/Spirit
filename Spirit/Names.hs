module Spirit.Names (nameList, filteredNameList) where

import Control.Monad (guard)

-- Produce an infinite list of unique names, which don't overlap with a given list
filteredNameList :: [String] -> [String]
filteredNameList xs = filterList xs nameList

-- Produce an infinite list of unique names
nameList :: [String]
nameList = [replicate k ['a' .. 'z'] | k <- [1..]] >>=
           sequence

-- Filter an infinite list by a finite one
filterList :: Eq a => [a] -> [a] -> [a]
filterList finite infinite = infinite >>=
                             \a -> guard (a `notElem` finite) >>
                             return a
