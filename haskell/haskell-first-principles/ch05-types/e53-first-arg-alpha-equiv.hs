--
-- Given a type, write the function (p 154)
-- Exercise 3, p 155.
--
-- Similar to the fst function for tuples.
--

c'' :: b -> a -> b
c'' x _ = x

--
-- ANSWER: Yes, the same thing as the function `c` from the previous exercise.
--
-- λ> c'' "Tomb Raider" "The Last Revelation"
-- "Tomb Raider"
--
