package main

import (
  "fmt"
  "os"
  "strings"
  "strconv"
  "math"

  "day_19/translation"
)

const debug = true
// const debug = false

const filename = "example_sm.txt"
// const filename = "example.txt"
// const filename = "input.txt"

type T = translation.Translation

type Reading struct {
  x int
  y int
  z int
}

type Sensor struct {
  readings []*Reading
  innerDistances []*Distance
  translationsToZero []T
}

type Distance struct {
  r1 *Reading
  r2 *Reading
  distance float64
}

func main() {
  data, _ := os.ReadFile(filename)
  trimmedData := strings.Trim(string(data), "\n ")
  allSensorReadings := strings.Split(trimmedData, "\n\n")

  sensors := loadSensorData(allSensorReadings)
  for _, sensor := range sensors {
    sensor.calculateDistances()
  }

  fmt.Println("readings:")
  fmt.Println(len(sensors[0].readings))
  fmt.Println("innerDistances:")
  for _, d := range sensors[0].innerDistances {
    fmt.Println(d)
  }
  fmt.Println("")
}

func loadSensorData(allSensorReadings []string) []*Sensor {
  sensors := make([]*Sensor, 0)

  for _, sensorReadings := range allSensorReadings {
    sensor := Sensor{}
    sensors = append(sensors, &sensor)

    readings := strings.Split(sensorReadings, "\n")
    // skip first line: --- scanner \d ---
    for i := 1; i < len(readings); i++ {
      coords := strings.Split(readings[i], ",")
      x, _ := strconv.Atoi(coords[0])
      y, _ := strconv.Atoi(coords[1])
      z, _ := strconv.Atoi(coords[2])

      reading := Reading{x, y, z }
      sensor.readings = append(sensor.readings, &reading)
    }
  }

  return sensors
}

func (s *Sensor) calculateDistances() {
  for i := 0; i < len(s.readings) - 1; i++ {
    for j := i + 1; j < len(s.readings); j++ {
      distance := Distance {
        r1: s.readings[i],
        r2: s.readings[j],
        distance: distanceBetween(s.readings[i], s.readings[j]),
      }

      s.innerDistances = append(s.innerDistances, &distance)
    }
  }
}

func distanceBetween(r1 *Reading, r2 *Reading) float64 {
  dx := float64(r2.x - r1.x)
  dy := float64(r2.y - r1.y)
  dz := float64(r2.z - r1.z)

  rawDistance := math.Sqrt(math.Pow(dx, 2) + math.Pow(dy, 2) + math.Pow(dz, 2))
  return round(rawDistance, 2)
}

func round(x float64, precision float64) float64 {
  multiplier := math.Pow(10, precision)
  return math.Floor(x * multiplier) / multiplier
}
