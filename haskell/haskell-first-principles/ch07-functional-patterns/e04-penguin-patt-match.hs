--
-- Pattern Matching, p 233.
--

data WherePenguinsLive =
    Galapagos
  | Antarctica
  | Australia
  | SouthAfrica
  | SouthAmerica
  deriving (Eq, Show)

data Penguin =
  Location WherePenguinsLive
  deriving (Eq, Show)

isSouthAfrica :: WherePenguinsLive -> Bool
isSouthAfrica SouthAfrica = True
isSouthAfrica _           = False

gimmeWhereTheyLive :: Penguin -> WherePenguinsLive
gimmeWhereTheyLive (Location whereitlives) = whereitlives


galapagosPenguin :: Penguin -> Bool
galapagosPenguin (Location Galapagos) = True
galapagosPenguin _                = False

antarcticPenguin :: Penguin -> Bool
antarcticPenguin (Location Antarctica) = True
antarcticPenguin _                     = False

antarcticOrGalapagos :: Penguin -> Bool
antarcticOrGalapagos p =
     (galapagosPenguin p)
  || (antarcticPenguin p)


humbolt = Location SouthAfrica
gentoo = Location Antarctica
macaroni = Location Antarctica
little = Location Australia
galapagos = Location Galapagos


main :: IO ()
main = do
  putStrLn $ show (gimmeWhereTheyLive gentoo)


--
--    λ> isSouthAfrica (gimmeWhereTheyLive humbolt)
--    True
--    λ> isSouthAfrica (gimmeWhereTheyLive gentoo)
--    False
--
--    λ antarcticPenguin gentoo
--    True
--    λ antarcticOrGalapagos gentoo
--    True
--    λ antarcticOrGalapagos galapagos
--    True
--    λ antarcticOrGalapagos humbolt
--    False
--
