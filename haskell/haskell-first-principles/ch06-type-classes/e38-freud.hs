--
-- EXERCISES: Match the types, p 211.
-- Exercise 5, p 211.
--

-- freud :: a -> a
freud :: Ord a => a -> a
freud x = x

--
-- Works. We can change
--
--    freud :: a -> a
--
-- to
--
--    freud :: Ord a => a -> a
--
-- because we do nothing especial with x. Any value that has an
-- instance of Ord will do when we actually use freud.
--

