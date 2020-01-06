-- area2.hs

area d = pi * (r * r)
  where r = d / 2

main :: IO ()
main = do
  print (area 9)
  print (area 6)

--
-- In ghci:
--
--    :load area2.hs
--    main
--
