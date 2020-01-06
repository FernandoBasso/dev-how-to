
fn :: (a, b) -> (c, d) -> ((b, d), (a, c))
fn tupA tupB =
  ((snd tupA, snd tupB), (fst tupA, fst tupB))

--
-- :load tuple2.hs
-- *Main> fn ("master", "yoda") ("jedi", "tux")
-- (("yoda","tux"),("master","jedi"))
