module Learn where

x = 10 * 5 + y
r = x * 5
y = 10

f x =
  let y = x * 2
      z = x ^ 2
  in 2 * y * z

g h =
  let l = "Secura"
  in h ++ " " ++ l
--
-- λ> g "Aayla"
-- "Aayla Secura"
----
