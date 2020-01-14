--
-- EXERCISES: Match the types, p 211.
-- Exercise 8, p 212.
--

myX = 1 :: Int
sigmund' :: Int -> Int
-- sigmund' :: Num a => a -> a
sigmund' x = myX

--
-- Nope, can't go from the specifc Int to the generic Numa a => a.
--

