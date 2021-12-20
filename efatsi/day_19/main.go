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

const filename = "example.txt"
// const filename = "input.txt"

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

  for _, s := range sensors {
    fmt.Println(s.name)
    fmt.Println(s.translationsToBase)
    fmt.Println(s.offsetToBase)
    fmt.Println("")
  }
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
                s2.offsetToBase = calculateOffsetToBase(translation, s1.offsetToBase, d1.r1, successMap[d1.r1][i])
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

func calculateOffsetToBase(translation Translation, s1Offset [3]int, s1Hub *Reading, s2Distances []*Distance) [3]int {
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

  x, y, z := translation(s2Hub.coordinates())
  offset := [3]int{
    s1Offset[0] + s1Hub.x - x,
    s1Offset[1] + s1Hub.y - y,
    s1Offset[2] + s1Hub.z - z,
  }

  if debug {
    fmt.Println("s1Hub: ", s1Hub.toString())
    fmt.Println("s2Hub: ", s2Hub.toString())
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

func contains(readings []*Reading, reading Reading) bool {
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
