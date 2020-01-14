--
-- EXERCISES: Match the types, p 211.
-- Exercise 4, p 211.
--

--f :: Float
f :: RealFrac a => a
f = 1.0

--
-- Works. Changing Float to RealFrac a => a is OK because RealFrac
-- implies Fractional, and 1.0 is inferred as Fractional.
--
