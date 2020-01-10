--
-- Given a type, write the function (p 154)
-- Exercise 6, p 155.
--
-- Looks like this one is about converting types.
--

co :: (b -> c) -> (a -> b) -> a -> c
co bToC aToB x = bToC (aToB x)

--
-- Since we don't have implementations of `bToC' and `aToB', we can at least
-- use `id' in their place.
--
-- λ> co (\x -> x) (\x -> x) "May the force..."
-- "May the force..."
-- λ> co id id "...be with you."
-- "...be with you."
--
