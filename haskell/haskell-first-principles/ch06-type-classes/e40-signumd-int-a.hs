--
-- EXERCISES: Match the types, p 211.
-- Exercise 7, p 212.
--

myX = 1 :: Int

sigmund :: Int -> Int
-- sigmund :: a -> a -- Nope, won't work.
sigmund x = myX

--
-- Nope. Can't go from a more concrete or specific type to a more
-- general one.
--

