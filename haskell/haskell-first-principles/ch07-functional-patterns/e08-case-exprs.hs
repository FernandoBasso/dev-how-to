--
-- Case Expressions, p 237.
--

--------------------------------------------------------------------------------
-- p 238.
--

x = 1

result :: String
result = if x + 1 == 1 then "AWESOME" else "wut‽"

funcZ x =
  case x + 1 == 1 of
    True -> "AWESOME"
    False -> "wut‽"
--
--    λ> putStrLn $ funcZ 0
--    AWESOME
--    λ> putStrLn $ funcZ 1
--    wut‽
--


--------------------------------------------------------------------------------
-- p 238.
--

pal xs =
  case xs == reverse xs of
    True  -> xs ++ " is palindrome"
    False -> xs ++ " is not palindrome"

--
-- NOTE: Could use _ to pattern match for anything in place of False.
--

pal' xs =
  case result of
    True  -> "Yes"
    False -> "Nope"
  where result = xs == reverse xs


--------------------------------------------------------------------------------
-- p 238.
--

greetIfCool :: String -> IO ()
greetIfCool coolness =
  case cool of
    True  -> putStrLn "Heyyy, What's shakin'‽"
    False -> putStrLn "Pshhh..."
  where cool = coolness == "downright frosty yo"

--
--    λ> greetIfCool "Hello"
--    Pshhh...
--    λ> greetIfCool "downright frosty yo"
--    Heyyy, What's shakin'‽
--

