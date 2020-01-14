--
-- EXERCISES: Match the types, p 211.
-- Exercise 10, p 212.
--

import Data.List (sort)

--young :: [Char] -> Char
young :: Ord a => [a] -> a
young xs = head (sort xs)

--
-- Yes, we can change from [Char] to Ord a because sort requires Ord
-- and head requires [a].
--


