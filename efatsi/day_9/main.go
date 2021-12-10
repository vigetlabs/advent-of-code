package main

import (
  "fmt"
  "os"
  "strings"
  "strconv"
)

const width = 10
const height = 5
const filename = "example.txt"

// const width = 100
// const height = 100
// const filename = "input.txt"

func main() {
  data, _ := os.ReadFile(filename)

  trimmedData := strings.Trim(string(data), "\n ")
  ventLines := strings.Split(trimmedData, "\n")

  heightmap := loadHeightmap(ventLines)

  lowPoints := make([]int, 0)
  for y := 0; y < height; y++ {
    for x := 0; x < width; x++ {
      reading := heightmap[y][x]

      if isLowpoint(heightmap, reading, x, y) {
        lowPoints = append(lowPoints, reading)
      }
    }
  }

  solvePartOne(lowPoints)
}

func solvePartOne(lowPoints []int) {
  riskLevel := 0
  for _, point := range lowPoints {
    riskLevel += point + 1
  }
  fmt.Println("Risk Level:", riskLevel)
}

func isLowpoint(heightmap [height][width]int, reading int, x int, y int) bool {
  // Check Up
  if y > 0 && heightmap[y-1][x] <= reading {
    return false
  }

  // Check Down
  if y < height - 1 && heightmap[y+1][x] <= reading {
    return false
  }

  // Check Left
  if x > 0 && heightmap[y][x-1] <= reading {
    return false
  }

  // Check Right
  if x < width - 1 && heightmap[y][x+1] <= reading {
    return false
  }

  // Else, all adjascent values are higher, we have a low point!
  return true
}

func loadHeightmap(ventLines []string) [height][width]int {
  var heightmap [height][width]int

  for y, line := range ventLines {
    ventReadings := strings.Split(line, "")

    for x, reading := range ventReadings {
      val, _ := strconv.Atoi(reading)
      heightmap[y][x] = val
    }
  }

  return heightmap
}
