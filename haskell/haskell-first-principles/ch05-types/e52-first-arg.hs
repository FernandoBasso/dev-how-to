--
-- Given a type, write the function (p 154)
-- Exercise 2, p 155.
--
-- Similar to the fst function for tuples.
--

c :: a -> b -> a
c x _ = x

--
-- λ> c "Tomb Raider" "1996"
-- "Tomb Raider"
--
