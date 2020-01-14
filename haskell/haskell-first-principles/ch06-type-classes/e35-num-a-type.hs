--
-- EXERCISES: Match the types, p 211.
-- Exercise 1, p 211.
--

i :: Num a => a
i = 1
--
-- Changing to
--
--    i :: a
--    i = 1
--
-- does not work because the literal 1 needs an instance of `Num a'.
--
-- `i :: a' means that `i' could be of any type. But 1 is not of any
-- type. It is one of the `Num' types. Numeric literal constants must
-- be of a `Num' type class or one of the numeric concrete types.
--
-- 1 is of type `Num a => a'. We cannot drop the type class
-- constraint.
--

