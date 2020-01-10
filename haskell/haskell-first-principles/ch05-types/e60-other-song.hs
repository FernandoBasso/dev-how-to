--
-- EXERCISES: Fix It, p 156.
-- Exercise 2, p 156.
--

module Sing where

fstString :: [Char] -> [Char]
fstString x = x ++ " in the rain"


sndString :: [Char] -> [Char]
sndString x = x ++ " twinkle little star... (Bio Shock)"

sing :: [Char]
sing = if (x > y) then fstString x else sndString y
  where x = "Singing"
        y = "Twinkle"

--
-- λ> sing
-- "Twinkle twinkle little star... (Bio Shock)"
--
