--
-- Pattern Matching, p 227.
--

isZero :: Integer -> Bool
isZero 0 = True
isZero _ = False

--
-- The order of the patterns matters! Order them from most specific to
-- least specific.
--


isOne :: Integer -> Bool
isOne 1 = True
--
-- Oops. Forgot to handle the cases when the parameter is not 1.
--
-- Incomplete pattern matches applied to data they don’t handle will
-- return bottom, a non-value used to denote that the program cannot
-- return a value or result.
--
-- Use `:set -Wall' to catch the problem before runtime.
--


