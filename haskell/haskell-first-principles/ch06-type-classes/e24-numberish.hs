--
-- Instances are dispatched by type, p 201.
--

--
-- Typeclasses are dispatched by type. Type classes are defined by the set of
-- operations and values all instances will provide. Type class instances are
-- unique pairings of the type class and a type. They define the ways to
-- implement the type class methods for that type.
--
-- • a type class defines a set of functions and/or values;
-- • types have instances of that type class;
-- • the instances specify the ways that type uses the functions of
--   the type class.

--
-- Just an example. Never actually do something like this, the book says.
--

--
-- NOTE: Looks like an interface in other languages to me, not the syntax, but
-- the semantics, as if the class is defining a contract that those who
-- implement it must comply with.
--

class Numberish a where
  fromNumber :: Integer -> a
  toNumber :: a -> Integer

--
-- Pretend newtype is data for now.
--
newtype Age =
  Age Integer
  deriving (Eq, Show)

instance Numberish Age where
  fromNumber n = Age n
  toNumber (Age n) = n

newtype Year =
  Year Integer
  deriving (Eq, Show)

instance Numberish Year where
  fromNumber n = Year n
  toNumber (Year n) = n

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
--    λ> :load dev.hs
--    λ> sumNumberish (Age 1) (age (-2))
--    Age (-1)
--    λ> :type sumNumberish
--    sumNumberish :: Numberish a => a -> a -> a
--
-- If we partially-apply, it knows that the concrete type of Numberish => a
-- must be Year.
--
--    λ> :type sumNumberish (Year 1)
--    sumNumberish (Year 1) :: Year -> Year
--
