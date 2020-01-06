fst' :: (a, b) -> a
fst' (a, _) = a

snd' :: (a, b) -> b
snd' (_, b) = b

tupFn :: (Int, [a])
      -> (Int, [a])
      -> (Int, [a])
tupFn (a, b) (c, d) = ((a + c), (b ++ d))

--
-- :load tuple1.hs
-- *Main> tupFn (10, "master") (20, " yoda")
-- (30,"master yoda")
--
