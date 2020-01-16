--
-- Higher-order functions, p 243.
--

data Employee =
  Coder | Manager | Veep | CEO
  deriving (Eq, Ord, Show)

reportBoss :: Employee -> Employee -> IO ()
reportBoss e e' =
  putStrLn $ show e ++ " is the boss of " ++ show e'

--
--    λ> reportBoss Manager Coder
--    Manager is the boss of Coder
--

employeeRank :: Employee -> Employee -> IO ()
employeeRank e e' =
  case compare e e' of
    GT -> reportBoss e e'
    EQ -> putStrLn "Neither employee is the boss"
    LT -> (flip reportBoss) e e' -- <1>
--
--    λ> employeeRank Coder Veep
--    Veep is the boss of Coder
--
--    λ> employeeRank Veep Coder
--    Veep is the boss of Coder
--
-- <1>: Could have just used
--
--    reportBoss e' e
--
-- That is, swap the order e and e' themselves instead of using flip
-- to swap the order of params of the reportBoss function.
--

