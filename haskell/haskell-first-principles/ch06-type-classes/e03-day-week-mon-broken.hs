{-# LANGUAGE NoMonomorphismRestriction #-}

data DayOfWeek =
  Mon | Tue | Wed | Thu | Fri | Sat | Sun
  deriving Show

-- Day of the week and day of the month.
data Date =
  Date DayOfWeek Int
  deriving Show

instance Eq DayOfWeek where
  (==) Mon Mon = True
  (==) Tue Tue = True
  (==) Wed Wed = True
  (==) Thu Thu = True
  (==) Fri Fri = True
  (==) Sat Sat = True
  (==) Sun Sun = True
--  (==) _ _ = False
--
-- Without the _ _ pattern we get a non-exhaustive pattern in function
-- == exeption. What we can do to catch this sort of problem before
-- runtime is to set `-Wall':
--
-- λ :set -Wall
-- λ :load dev.hs
-- [1 of 1] Compiling Main             ( dev.hs, interpreted )
--
-- dev.hs:13:3: warning: [-Wincomplete-patterns]
--     Pattern match(es) are non-exhaustive
--     In an equation for ‘==’:
--         Patterns not matched:
--             Mon Tue
--             Mon Wed
--             Mon Thu
--             Mon Fri
--             ...
--    |
-- 13 |   (==) Mon Mon = True
--    |   ^^^^^^^^^^^^^^^^^^^...
--

instance Eq Date where
  (==) (Date weekday dayOfMonth)
       (Date weekday' dayOfMonth') =
    weekday == weekday' && dayOfMonth == dayOfMonth'

--
-- Can compare dayOfWeek with prefix and infix:
--
--     λ> Fri == Fri
--     True
--     λ> Fri /= Fri
--     False
--
-- Can compare Date with prefix and infix:
--
--     λ> Date Wed 3 /= Date Wed 3
--     False
--     λ> (==) (Date Fri 1) (Date Fri 1)
--     True
--
-- Have to use parens because of precedence. When comparison operator is
-- in the middle, no need for parens.
--
-- Can display/show/print:
--
--     λ> Sun
--     Sun
--     λ> Date Sun 13
--     Date Sun 13
--
-- Can partially apply it:
--
--
-- λ> eqFri5 = (==) (Date Fri 5)
-- λ> eqFri5 (Date Fri 4)
-- False
-- λ> eqFri5 (Date Fri 5)
-- True
--

