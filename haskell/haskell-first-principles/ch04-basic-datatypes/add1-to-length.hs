
--
-- We want a function that adds 1 to the length
-- of a string argument and returns that result.
--

op = (+)

add1 xs = op w 1
  where w = length xs

--
-- Prelude> :load add1-to-length.hs
-- *Main> add1 "foo"
-- 4
-- *Main> add1 "jedi"
-- 5
-- *Main>
--
