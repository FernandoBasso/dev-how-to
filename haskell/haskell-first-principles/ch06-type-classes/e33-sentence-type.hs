--
-- Chapter Exercises - Does it typecheck? p 209.
-- Instruction example, p 209.
--

type Subject = String
type Verb = String
type Object = String

data Sentence =
  Sentence Subject Verb Object
  deriving (Eq, Show)

s1 = Sentence "dogs" "drool"
s2 = Sentence "Julie" "loves" "dogs"

--
-- If we try to evaluate s1, we get:
--
--    No instance for (Show (Object -> Sentence))
--
-- Of course, Data Constructors, when partially-applied (as we did above)
-- return a function, and the function type constructor (->) can't have a
-- show instance.
--
-- Otherwise, yes, the code typechecks.
--
