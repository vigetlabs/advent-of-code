package main

import (
  "fmt"
  "os"
  "strings"
  "strconv"

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
  innerDistances []*DistanceBetween
  translationsToZero []T
}

func main() {
  data, _ := os.ReadFile(filename)
  trimmedData := strings.Trim(string(data), "\n ")
  allSensorReadings := strings.Split(trimmedData, "\n\n")

  sensors := loadSensorData(allSensorReadings)

  fmt.Println(len(sensors[0].readings))
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
