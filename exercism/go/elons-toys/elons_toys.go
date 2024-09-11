package elon

import "fmt"

// Drive drives the car and updates its battery and distance travelled.
func (car *Car) Drive() {
	if (*car).battery-(*car).batteryDrain < 0 {
		return
	}

	(*car).battery -= (*car).batteryDrain
	(*car).distance += (*car).speed
}

// DisplayDistance displays information about the distance travelled.
func (car *Car) DisplayDistance() string {
	return fmt.Sprintf("Driven %d meters", (*car).distance)
}

// DisplayBattery displays information about the battery.
func (car *Car) DisplayBattery() string {
	return fmt.Sprintf("Battery at %d%%", (*car).battery)
}

// CanFinish returns a bool indicating the car has enough
// battery to finish the race.
func (car Car) CanFinish(trackDistance int) bool {
	timesCanDrive := car.battery / car.batteryDrain
	distanceCanDrive := car.speed * timesCanDrive

	return distanceCanDrive >= trackDistance
}
