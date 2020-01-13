--
-- (+) and (-) are provided by Num. (/) is not.
--

-- divideThenAdd :: Num a => a -> a -> a
divideThenAdd :: Fractional a => a -> a -> a
divideThenAdd x y = (x / y) + 1

--
-- No need for the constraint to be
--
--    (Num a, Fractional a)
--
-- because Fractional inherits from Num.
--
