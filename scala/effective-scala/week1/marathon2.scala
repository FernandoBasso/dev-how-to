package Marathon2

def marathonDuration(speed: Double): Double =
  val distanceKM = 42.195
  val durationSecs = distanceKM / speed
  durationSecs * 60

//
// We can name the parameters to make things more self-documenting.
//
def aliceDuration: Double = marathonDuration(speed = 14)
def bobDuration: Double = marathonDuration(speed = 12)

@main def marathon1(): Unit =
  println(f"${"Alice"}%5s: $aliceDuration%6.2f")
  println(f"${"Bob"}%5s: $bobDuration%8.6f")
