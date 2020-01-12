{-# LANGUAGE NoMonomorphismRestriction #-}

f :: Int -> Bool
f 2 = True
--
-- What when the input to f is not 2‽ Another companion cube is
-- destroyed.
--
--     λ :set -Wall
--     λ :load dev.hs
--     [1 of 1] Compiling Main             ( dev.hs, interpreted )
--
--     dev.hs:4:1: warning: [-Wincomplete-patterns]
--         Pattern match(es) are non-exhaustive
--         In an equation for ‘f’:
--             Patterns not matched: p where p is not one of {2}
--       |
--     4 | f 2 = True
--       | ^^^^^^^^^^
--

