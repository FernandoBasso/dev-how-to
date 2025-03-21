package Catthullhu

enum Role:
  case
    Catcrobat
  , Pussyfoot
  , Scraper
  , TigerDreamer
  , Twofootologist

case class Background(description: String, other: String)

enum Lifestyle:
  case Feral, Housecat, Showcat

case class Cat(
  name: String,
  role: Role,
  background: Background,
  description: String,
)
