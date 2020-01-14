--
-- Chapter Exercises - Does it typecheck? p 208.
-- Exercise 1, p 209.
--

data Person = Person Bool

--
-- Fixed:
--
--    data Person = Person Bool deriving Show
--

printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)

--
-- Does not typecheck because Person does not have an instance of Show.
--
