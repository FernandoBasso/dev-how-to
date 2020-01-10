--
-- Given a type, write the function (p 154)
-- Exercise 8, p 155.
--

a' :: (a -> b) -> a -> b
a' aToB x = aToB x

--
-- λ> a' (\x -> x) "Thy false promise..."
-- "Thy false promise..."
--

--
-- Since we don't have an `aToB' function, we used an identity one.
--
