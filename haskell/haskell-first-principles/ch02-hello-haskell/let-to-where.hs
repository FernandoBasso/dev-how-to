{-
let x = 7
    y = negate x
    z = y * 10
in z / x + y
-}

result1 =
  let x = 7
      y = negate x
      z = y * 10
      in z / x + y
-- -17

-- My experiment
result2 =
  let x = 1
      y = 2
      z = 3
      in x + y + z
-- 6

result1Where =
  z / x + y
  where
    x = 7
    y = negate x
    z = y * 10
-- 17
