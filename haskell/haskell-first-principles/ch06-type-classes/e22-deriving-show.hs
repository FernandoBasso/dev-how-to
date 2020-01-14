--
-- Show, p 200.
--

--
-- We can simply derive the Show instance because it is one of the type
-- classes GHC supports deriving instances for by default.
--

data Mood = Blah deriving Show
