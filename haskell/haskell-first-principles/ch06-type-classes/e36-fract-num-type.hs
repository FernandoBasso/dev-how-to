--
-- EXERCISES: Match the types, p 211.
-- Exercise 2, p 211.
--

f :: Float
f = 1.0

--
-- Changing the constraint
--
--     λ> i = 1.0
--     λ> :t i
--     i :: Fractional p => p
--
-- 1.0 is inferred a Fractional. We cannot go from a more specific
-- type class to a more general one. That is, we can't go from
-- Fractional or Float to Num.
--


