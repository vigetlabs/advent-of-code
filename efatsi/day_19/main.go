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

type Translation = translation.Translation

type Reading struct {
  x int
  y int
  z int
}

type Sensor struct {
  readings []*Reading
  innerDistances []*Distance
  translationsToZero []Translation
}

type Distance struct {
  r1 *Reading
  r2 *Reading
  dx int
  dy int
  dz int
  distance float64
}

// TODO
// while running over distances, make map of readings involved in matches
// as soon as there are 11 in both, game over, tally it up, get the offset

func main() {
  data, _ := os.ReadFile(filename)
  trimmedData := strings.Trim(string(data), "\n ")
  allSensorReadings := strings.Split(trimmedData, "\n\n")

  sensors := loadSensorData(allSensorReadings)
  for _, sensor := range sensors {
    sensor.calculateDistances()
  }

  // Hardcode the first sensor's translation to the no-op transform
  // Others will lean on this to build their translation chain
  sensors[0].translationsToZero = []Translation{translation.Translate1}
  for i := 0; i < len(sensors) - 1; i++ {
    for j := i + 1; j < len(sensors); j++ {
      s1 := sensors[i]
      s2 := sensors[j]

      attemptTranslationFind(s1, s2)
      if len(s2.translationsToZero) >= 1 {
        fmt.Println("Found Translation For", i, s1.translationsToZero)
        fmt.Println("Found Translation For", j, s2.translationsToZero)
      }
    }
  }
}

func attemptTranslationFind(s1 *Sensor, s2 *Sensor) {
  // successMap is dense: A map w/ Reading keys (from s1), which point to
  //   a map w/ translation index keys and []*Distance values.
  // Purpose: for each hub node, track how many times a tranaslation function
  //   succeeded. Once you get to 11, you know there are 12 points who's
  //   distances from the hub shared the same translation.
  successMap := make(map[*Reading]map[int][]*Distance, 0)

  fmt.Println("s1.innerDistances", len(s1.innerDistances))
  fmt.Println("s2.innerDistances", len(s2.innerDistances))

  for _, d1 := range s1.innerDistances {
    _, exists := successMap[d1.r1]
    if !exists {
      successMap[d1.r1] = make(map[int][]*Distance, 0)
    }

    for _, d2 := range s2.innerDistances {
      if (d1.distance == d2.distance) {
        for i, translation := range translation.AllTranslations {
          // check if distance between s2 points is translatable to s1 points
          tx, ty, tz := translation(d2.dx, d2.dy, d2.dz)
          straightMatch := (tx == d1.dx && ty == d1.dy && tz == d1.dz)
          reverseMatch := (-tx == d1.dx && -ty == d1.dy && -tz == d1.dz)

          if (straightMatch || reverseMatch) {
            if debug {
              x, y, z := translation(1,2,3)
              fmt.Println("Got one", x, y, z)
              fmt.Println("d1", d1.r1.toString(), "-" + d1.r2.toString())
              fmt.Println("d2", d2.r1.toString(), "-" + d2.r2.toString())
              fmt.Println("")
            }

            matches, exists := successMap[d1.r1][i]
            if exists {
              successMap[d1.r1][i] = append(matches, d2)
              if len(successMap[d1.r1][i]) == 11 {
                s2.translationsToZero = append([]Translation{translation}, s1.translationsToZero...)
                return
              }
            } else {
              successMap[d1.r1][i] = []*Distance{d2}
            }
            break
          }
        }
      }
    }
  }

  fmt.Println("Didn't make it")
  fmt.Println("successMap", successMap)
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
      r1 := s.readings[i]
      r2 := s.readings[j]

      distance := Distance {
        r1: r1,
        r2: r2,
        dx: r2.x - r1.x,
        dy: r2.y - r1.y,
        dz: r2.z - r1.z,
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

func (r *Reading) toString() string {
  return strconv.Itoa(r.x) + "," + strconv.Itoa(r.y) + "," + strconv.Itoa(r.z)
}
