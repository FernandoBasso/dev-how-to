--
-- Given a type, write the function (p 154)
-- Exercise 4, p 155.
--
-- Similar to `snd' for tuples.
--

c' :: a -> b -> b
c' _ y = y

--
-- λ> c' "Tomb Rider" "The Angel of Darkness"
-- "The Angel of Darkness"
--
