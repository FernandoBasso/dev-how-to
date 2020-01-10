--
-- Given a type, write the function (p 154)
-- Exercise 7, p 155.
--

a :: (a -> c) -> a -> a
a _ x = x


--
-- The function (a -> c) is not used, but we must still passe it when applying
-- `a' because it is required by the type signature. Ends up being like the `id'
-- function.
--
-- λ> a (\y -> y) 1
-- 1
-- λ> a id 1
-- 1
--
