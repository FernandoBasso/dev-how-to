{-# LANGUAGE NoMonomorphismRestriction #-}

--
-- This function does _not_ produce a non-exhaustive exception.
--

intToBool :: Int -> Bool
intToBool 0 = True
intToBool _ = False

--
-- Zero means success/true. Any other int means some sort of
-- failure/false.
--
--     λ :set -Wall
--     λ :load dev.hs
--     λ intToBool (-1)
--     False
--     λ intToBool 13
--     False
--     λ intToBool 0
--     True
--
--
--     λ> minBound :: Int
--     -9223372036854775808
--     λ> maxBound :: Int
--     9223372036854775807
--
