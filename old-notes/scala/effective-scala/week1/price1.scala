package Price1

def showPrice(paintingArea: Double, paintPrice: Double): String =
  val price = paintingArea * paintPrice
  if price > 100 then
    "Too expensive..."
  else if price < 10 then
    "So inexpensive..."
  else
    price.toString

@main def main(): Unit =
  println(showPrice(paintingArea = 24, paintPrice = 4))
  println(showPrice(paintingArea = 24, paintPrice = 5))
