--
-- EXERCISES: Type-Known-Do, p 156.
-- Exercise 4, p 159.
--

munge :: (x -> y) -> (y -> (w, z)) -> x -> w
munge xToY yToWZ x =
  fst (yToWZ (xToY x))
