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
  (==) _ _     = False

instance Eq Date where
  (==) (Date weekday dayOfMonth)
       (Date weekday' dayOfMonth') =
    weekday == weekday' && dayOfMonth == dayOfMonth'
--
-- Can partially apply it:
--
--
--
-- λ> eqFri5 = (==) (Date Fri 5)
-- λ> eqFri5 (Date Fri 4)
-- False
-- λ> eqFri5 (Date Fri 5)
-- True
--
