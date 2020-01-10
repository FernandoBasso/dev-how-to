--
-- EXERCISES: Fix It, p 156.
-- Exercise 1, p 156.
--

-- <1>
module Sing where

--                 <2>
fstString :: [Char] -> [Char]
fstString x = x ++ " in the rain"

--                     <3>
sndString :: [Char] -> [Char]
sndString x = x ++ " over the rainbow"

sing :: [Char] --                   <4>
sing = if (x > y) then fstString x else sndString y
  where x = "Singing"
        y = "Somewhere"

--
-- <1>: Modules name must start with an uppercase letter.
--
-- <2>: `->' is the function constructor, not `++'.
--
-- <3>: The function is clearly returning a list of `Char'. Therefore, the
-- type signature should return [Char], not simply Char.
--
-- <4>: The syntax is `else', not `or' for an `if' clause.
--

--
-- λ> sing
-- "Somewhere over the rainbow"
--

--
-- NOTE: s1 > s2 does NOT compare string length, but rather, their values as
-- per ASCII/UTF-8 values.
--
