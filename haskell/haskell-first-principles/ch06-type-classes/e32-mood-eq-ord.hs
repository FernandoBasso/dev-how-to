--
-- Chapter Exercises - Does it typecheck? p 209.
-- Instruction example, p 209.
--

data Mood = Blah
          | Woot deriving (Eq, Show)

settleDown x = if x == Woot
                then Blah
                else x


--
-- Acceptable value are _only_ Blah and Woot from the Mood type.
--
--    λ> settleDown Blah
--    Blah
--    λ> settleDown Woot
--    Blah
--    λ> settleDown 9
--
--    <interactive>:19:12: error:
--        • No instance for (Num Mood) arising from the literal ‘9’
--        • In the first argument of ‘settleDown’, namely ‘9’
--          In the expression: settleDown 9
--          In an equation for ‘it’: it = settleDown 9
--
-- Because when we compared x == Woot, GHC infered that the only type that
-- contains those values is Mood, so, it only works with Mood values.
--
-- Can't use Blah > Woot because Mood does not have an instance of Ord (and
-- Eq, implicitly)
--
