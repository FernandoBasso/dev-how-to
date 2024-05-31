def marathonDuration(speed: Double): Double =
  val distanceKM = 42.195
  val durationSecs = distanceKM / speed
  durationSecs * 60

def aliceDuration: Double = marathonDuration(14)
def bobDuration: Double = marathonDuration(12)

//
// Using "${<some name>}" to avoid having to store them in
// val just to be able to right align them.
//

@main def marathon1(): Unit =
  println(f"${"Alice"}%5s: $aliceDuration%6.2f")
  println(f"${"Bob"}%5s: $bobDuration%8.6f")
