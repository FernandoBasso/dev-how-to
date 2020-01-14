--
-- EXERCISES: Match the types, p 211.
-- Exercise 9, p 212.
--

import Data.List (sort)

--jung :: Ord a => [a] -> a
jung :: [Int] -> Int
jung xs = head (sort xs)

--
-- Yes, we can go from the generic
--
--    Ord a => [a] -> a to
--
-- to the specific
--
--    [Int] -> Int
--

