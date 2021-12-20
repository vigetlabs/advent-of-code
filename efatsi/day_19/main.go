package main

import (
  "fmt"
  "os"
  "strings"
  "strconv"
  "math"

  "day_19/translation"
)

// const debug = true
const debug = false

// const filename = "example_sm.txt"
// const filename = "example.txt"
const filename = "input.txt"

type Translation = translation.Translation

type Reading struct {
  x int
  y int
  z int
}

type Sensor struct {
  name string
  readings []*Reading
  innerDistances []*Distance
  translationsToBase []Translation
  offsetToBase [3]int
}

type Distance struct {
  r1 *Reading
  r2 *Reading
  dx int
  dy int
  dz int
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

  // Hardcode the first sensor's translation & offsets.
  // Others will lean on these to build their own translation chain / offsets
  sensors[0].translationsToBase = []Translation{translation.Translate1}
  sensors[0].offsetToBase = [3]int{0, 0, 0}
  findTranslations(sensors)

  if debug {
    for _, s := range sensors {
      fmt.Println(s.name)
      fmt.Println(s.translate(1, 2, 3))
      fmt.Println(s.offsetToBase)
      fmt.Println("")
    }
  }

  solvePartOne(sensors)
  solvePartTwo(sensors)
}

func solvePartOne(sensors []*Sensor) {
  finalReadings := sensors[0].readings
  for _, sensor := range sensors[1:] {
    if debug { fmt.Println("Translating readings from", sensor.name) }

    for _, reading := range sensor.readings {
      translatedReading := sensor.translateToBase(reading)

      if !contains(finalReadings, translatedReading) {
        if debug { fmt.Println("Found a new one:", translatedReading.toString()) }
        finalReadings = append(finalReadings, &translatedReading)
      }
    }
  }

  fmt.Println("Part 1", len(finalReadings))
}

func solvePartTwo(sensors []*Sensor) {
  maxDistance := 0

  for i := 0; i < len(sensors); i++ {
    o1 := sensors[i].offsetToBase

    for j := 0; j < len(sensors); j++ {
      if i == j { continue }

      o2 := sensors[j].offsetToBase

      manhanttanDistance := abs(o1[0] - o2[0]) + abs(o1[1] - o2[1]) + abs(o1[2] - o2[2])
      if manhanttanDistance > maxDistance {
        if (debug) {
          fmt.Println("Found new max:")
          fmt.Println(o1)
          fmt.Println(o2)
          fmt.Println((o1[0] - o2[0]), (o1[1] - o2[1]), (o1[2] - o2[2]))
          fmt.Println(manhanttanDistance)
          fmt.Println("")
        }

        maxDistance = manhanttanDistance
      }

    }
  }

  fmt.Println("Part 2", maxDistance)
}

func findTranslations(sensors []*Sensor) {
  // recurse until we have no more missing translations
  hasTranslations, missingTranslations := splitOnTranslations(sensors)

  if (len(missingTranslations) > 0) {
    if debug {
      fmt.Println("hasTranslations    ", hasTranslations)
      fmt.Println("missingTranslations", missingTranslations)
      fmt.Println("")
    }

    for _, baseSensor := range hasTranslations {
      for _, nextSensor := range missingTranslations {
        if debug { fmt.Println("Attempting translation w/", []*Sensor{baseSensor}, "->", []*Sensor{nextSensor}) }
        attemptTranslationFind(baseSensor, nextSensor)

        if nextSensor.hasTranslation() {
          hasTranslations = append(hasTranslations, nextSensor)

          if debug {
            fmt.Println("Base Translation", baseSensor.translationsToBase)
            fmt.Println("Next Translation", nextSensor.translationsToBase)
          }
        }
      }
    }

    findTranslations(sensors)
  }
}

func splitOnTranslations(sensors []*Sensor) ([]*Sensor, []*Sensor) {
  // recalculate missings
  hasTranslations := make([]*Sensor, 0)
  missingTranslations := make([]*Sensor, 0)

  for _, s := range sensors {
    if s.hasTranslation() {
      hasTranslations = append(hasTranslations, s)
    } else {
      missingTranslations = append(missingTranslations, s)
    }
  }

  return hasTranslations, missingTranslations
}

func attemptTranslationFind(s1 *Sensor, s2 *Sensor) {
  // successMap is dense: A map w/ Reading keys (from s1), which point to
  //   a map w/ translation index keys and []*Distance values.
  // Purpose: for each hub node, track how many times a tranaslation function
  //   succeeded. Once you get to 11, you know there are 12 points who's
  //   distances from the hub shared the same translation.
  successMap := make(map[*Reading]map[int][]*Distance, 0)

  for _, d1 := range s1.innerDistances {
    _, exists := successMap[d1.r1]
    if !exists {
      successMap[d1.r1] = make(map[int][]*Distance, 0)
    }

    for _, d2 := range s2.innerDistances {
      if (d1.distance == d2.distance) {
        for i, translation := range translation.AllTranslations {
          tx, ty, tz := translation(d2.dx, d2.dy, d2.dz)
          straightMatch := (tx == d1.dx && ty == d1.dy && tz == d1.dz)
          reverseMatch := (-tx == d1.dx && -ty == d1.dy && -tz == d1.dz)

          if (straightMatch || reverseMatch) {
            if debug {
              x, y, z := translation(1,2,3)
              fmt.Println("Got one", x, y, z)
              fmt.Println("d1", d1.toString())
              fmt.Println("d2", d2.toString())
              fmt.Println("")
            }

            matches, exists := successMap[d1.r1][i]
            if exists {
              successMap[d1.r1][i] = append(matches, d2)
              if len(successMap[d1.r1][i]) == 11 {
                s2.translationsToBase = append([]Translation{translation}, s1.translationsToBase...)
                s2.offsetToBase = calculateOffsetToBase(s1, s2, d1.r1, successMap[d1.r1][i])
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

  if debug {
    fmt.Println("Didn't find one")
    fmt.Println("")
  }
}

func calculateOffsetToBase(s1 *Sensor, s2 *Sensor, s1Hub *Reading, s2Distances []*Distance) [3]int {
  var s2Hub *Reading
  // Check first pair of pairs, find common, that should be the hub of s2
  r11 := s2Distances[0].r1
  r12 := s2Distances[0].r2

  r21 := s2Distances[1].r1
  r22 := s2Distances[1].r2

  if r11 == r21 { s2Hub = r11 }
  if r11 == r22 { s2Hub = r11 }
  if r12 == r21 { s2Hub = r12 }
  if r12 == r22 { s2Hub = r12 }

  // Make sure hub is in every one (skip the first two)
  for _, d := range s2Distances[2:] {
    if s2Hub == nil { panic("missing hub!") }
    if (s2Hub != d.r1 && s2Hub != d.r2) {
      panic("missing incomplete hub!")
    }
  }

  // This took for goddamn ever
  x1, y1, z1 := s1.translate(s1Hub.coordinates())
  x2, y2, z2 := s2.translate(s2Hub.coordinates())
  offset := [3]int{
    s1.offsetToBase[0] + x1 - x2,
    s1.offsetToBase[1] + y1 - y2,
    s1.offsetToBase[2] + z1 - z2,
  }

  if debug {
    fmt.Println("s1Hub:    ", s1Hub.toString())
    fmt.Println("s2Hub:    ", s2Hub.toString())
    fmt.Println("s1Offset: ", s1.offsetToBase)
    fmt.Println("Offset:", offset)
  }

  return offset
}

func loadSensorData(allSensorReadings []string) []*Sensor {
  sensors := make([]*Sensor, 0)

  for _, sensorReadings := range allSensorReadings {
    sensor := Sensor{}
    sensors = append(sensors, &sensor)

    readings := strings.Split(sensorReadings, "\n")
    sensor.name = readings[0]
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

func contains(readings []*Reading, r Reading) bool {
  for _, e := range readings {
    if e.x == r.x && e.y == r.y && e.z == r.z {
      return true
    }
  }

  return false
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

func (sensor *Sensor) translate(x int, y int, z int) (int, int, int) {
  for _, translate := range sensor.translationsToBase {
    x, y, z = translate(x, y, z)
  }

  return x, y, z
}

func (sensor *Sensor) translateToBase(reading *Reading) Reading {
  x, y, z := sensor.translate(reading.coordinates())
  offset := sensor.offsetToBase

  translatedReading := Reading {
    offset[0] + x,
    offset[1] + y,
    offset[2] + z,
  }

  return translatedReading
}

func (sensor *Sensor) hasTranslation() bool {
  return len(sensor.translationsToBase) >= 1
}

func (d *Distance) toString() string {
  return d.r1.toString() + " -> " + d.r2.toString()
}

func (r *Reading) toString() string {
  return strconv.Itoa(r.x) + "," + strconv.Itoa(r.y) + "," + strconv.Itoa(r.z)
}

func (r *Reading) coordinates() (int, int, int) {
  return r.x, r.y, r.z
}

func abs(x int) int {
  if x >= 0 {
    return x
  } else {
    return x * -1
  }
}
