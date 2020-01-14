--
-- Show, p 196.
--

--
-- myVal is a string value.
--
--    myVal :: String
--

--
-- inputVal is a means/method of _obtaining_ a string value by performing
-- an I/O (input/output) effect.
--
--    inputVal :: IO String
--

data Mood = Blah

--
-- Must implement Show for Mood if we want to be able to print Blah.
--

instance Show Mood where
  show _ = "Blah"

--
--    λ> :load dev.hs
--    [1 of 1] Compiling Main             ( dev.hs, interpreted )
--    Ok, one module loaded.
--    λ> Blah
--    Blah
--
