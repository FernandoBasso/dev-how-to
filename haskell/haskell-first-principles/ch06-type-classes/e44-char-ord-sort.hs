--
-- EXERCISES: Match the types, p 211.
-- Exercise 11, p 212.
--

import Data.List (sort)

mySort :: [Char] -> [Char]
mySort = sort

signifier :: [Char] -> Char
--signifier :: Ord a => [a] -> a -- Nope...
signifier xs = head (mySort xs)

--
-- No, it doesn't work because mySort needs [Char], which is more
-- specific, and Ord a => [a] is more generic.
--

