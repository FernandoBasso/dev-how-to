--
-- Chapter Exercises - Does it typecheck? p 209.
-- Exercise 2, p 209.
--

data Mood = Blah
          | Woot deriving (Show)

--
-- Fixed:
--
--    data Mood = Blah
--              | Woot deriving (Eq, Show)
--

settleDown x = if x == Woot
                then Blah
                else x


--
-- Does not typecheck because can't use (==) without an instance of Eq.
--
