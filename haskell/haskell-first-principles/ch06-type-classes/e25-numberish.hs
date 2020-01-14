--
-- Instances are dispatched by type, p 203.
--

--
-- This is the even worse version than the previous one.
--

class Numberish a where
  fromNumber    :: Integer -> a
  toNumber      :: a -> Integer
  defaultNumber :: a

newtype Age =
  Age Integer
  deriving (Eq, Show)

instance Numberish Age where
  fromNumber n     = Age n
  toNumber (Age n) = n
  defaultNumber    = Age 65

newtype Year =
  Year Integer
  deriving (Eq, Show)

instance Numberish Year where
  fromNumber n      = Year n
  toNumber (Year n) = n
  defaultNumber     = Year 1998

--
-- Now a function that uses that stuff.
--
sumNumberish :: Numberish a => a -> a -> a
sumNumberish a a' = fromNumber summed
  where
    integerOfA      = toNumber a
    integerOfAPrime = toNumber a'
    summed = integerOfA + integerOfAPrime

--
-- It is not producing the error like in the book... It should be
-- impossible to Show it, but it doing something I don't understand
-- yet.
--
--    λ> defaultNumber
--    defaultNumber :: Numberish a => a
--
-- It does not produce the error. It just says it is `Numberish a =>
-- a'. Yet, it does understand the data constructor.
--
--    λ> defaultNumber :: Age
--    Age 65
--    λ> defaultNumber :: Year
--    Year 1998
--

--
-- Numberish is too arbitrary. Type classes should have laws and rules
-- about how they work. More about this when we study Monoid.
--

