--
-- Ord instances, p 191.
--

data DayOfWeek =
  Mon | Tue | Wed | Thu | Fri | Sat | Sun
  deriving (Show)

--
-- In order for DayOfWeek be able to have an Ord instance, it also needs to
-- have an instance of Eq.
--

instance Eq DayOfWeek where
  (==) Mon Mon = True
  (==) Tue Tue = True
  (==) Wed Wed = True
  (==) Thu Thu = True
  (==) Fri Fri = True
  (==) Sat Sat = True
  (==) Sun Sun = True
  (==) _ _     = False

--
-- Let's make Friday greater than any other day.
--
instance Ord DayOfWeek where
  compare Fri Fri = EQ
  compare Fri _   = GT
  compare _ Fri   = LT
  compare _ _     = EQ

--
-- Fri is always greater than any other day (but with our implementation
-- any other day is EQ to any other day.)
--
--    λ> compare Fri Sat
--    GT
--    λ> Fri > Sat
--    True
--    λ> fri < Sat
--    λ> Fri < Sat
--    False
--    λ> Fri < Mon
--    False
--
-- But the other days are always considered equal.
--
--    λ> Mon > Tue
--    False
--    λ> Tue > MonT
--    False
--
-- The equality operator == is fine, though, because we derived it.
--
--    λ> Mon == Tue
--    False
--    λ> Wed == Wed
--    True
--    λ> Fri == Fri
--    True
--
