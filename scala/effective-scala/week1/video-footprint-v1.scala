package VideoFootprintV1

case class Experience(
  duration: Int, // Seconds.
  definition:  Double, // Megabytes per second.
  network: Network,
)

enum Network:
  case Mobile, Fixed

val lowQuality  = 0.3 // Bitrate MB/s
val highQuality = 0.6 // Bitrate MB/s

val thirtyMinutes = 30 * 60 // thirty minutes in seconds

val highQualityMobile =
  Experience(thirtyMinutes, highQuality, Network.Mobile)

val lowQualityFixed =
  Experience(thirtyMinutes, lowQuality, Network.Fixed)

val dataCenterEnergy = 0.000072
val kgCO2PerKwh = 0.5

def networkEnergy(network: Network): Double =
  network match
    case Network.Mobile => 0.00088
    case Network.Fixed  => 0.00043
  
def footprint(experience: Experience): Double =
  val megabytes = experience.duration * experience.definition
  val energy    = dataCenterEnergy + networkEnergy(experience.network)
  energy * megabytes * kgCO2PerKwh

@main def main(): Unit =
  println(footprint(highQualityMobile))
  println(footprint(lowQualityFixed))
