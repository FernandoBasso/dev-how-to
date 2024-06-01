package VideoFootprintV2

type Seconds = Int
type MegabytesPerSecond = Double

enum Network:
  case Mobile, Fixed

case class Experience(
  duration: Seconds,
  definition:  MegabytesPerSecond,
  network: Network,
)

val lowQualityMBs: MegabytesPerSecond  = 0.3 // Bitrate MB/s
val highQualityMBs: MegabytesPerSecond = 0.6 // Bitrate MB/s

val thirtyMinutesInSeconds = 30 * 60 // thirty minutes in seconds

val highQualityMobile =
  Experience(thirtyMinutesInSeconds, highQualityMBs, Network.Mobile)

val lowQualityFixed =
  Experience(thirtyMinutesInSeconds, lowQualityMBs, Network.Fixed)

val dataCenterEnergyKwh = 0.000072
val kgCO2PerKwh = 0.5

def networkEnergy(network: Network): Double =
  network match
    case Network.Mobile => 0.00088
    case Network.Fixed  => 0.00043

def footprint(experience: Experience): Double =
  val energy    = dataCenterEnergyKwh + networkEnergy(experience.network)
  val megabytes = experience.duration * experience.definition
  energy * megabytes * kgCO2PerKwh

@main def main(): Unit =
  println(footprint(highQualityMobile))
  println(footprint(lowQualityFixed))
