--
-- EXERCISES: Type-Known-Do, p 156.
-- Exercise 2, p 158.
--

data A
data B
data C

q :: A -> B
q = undefined

w :: B -> C
w = undefined

e :: A -> C
e a = w (q a)
