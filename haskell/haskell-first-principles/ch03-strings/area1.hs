-- area.hs

area d = pi * (r * r)

-- Because of the two-pass compilation thing in haskell,
-- r is available even in code above its definition.
r = d / 2

d = 6

--
-- In ghci:
--
--    :load area.hs
--    area 6
--
