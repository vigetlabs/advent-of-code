package main

import (
  "fmt"
  "os"
  "strings"
  "strconv"
  // "sort"
)

type Dots [][]byte

const debug = true
const filename = "example.txt"

// const debug = false
// const filename = "input.txt"

func main() {
  data, _ := os.ReadFile(filename)
  trimmedData := strings.Trim(string(data), "\n ")

  inputs := strings.Split(trimmedData, "\n\n")
  pointsStr := inputs[0]
  instructionsStr := inputs[1]

  points := strings.Split(pointsStr, "\n")
  instructions := strings.Split(instructionsStr, "\n")

  maxX, maxY := countMaxDimensions(points)

  var dots Dots
  for x := 0; x < maxX + 1; x++ {
    column := make([]byte, maxY + 1)
    dots = append(dots, column)
  }

  for _, point := range points {
    x, y := pointToCoordinates(point)
    dots[x][y] = 1
  }

  fmt.Println("maxX", maxX)
  fmt.Println("maxY", maxY)
  fmt.Println("instructions", instructions)

  printDots(dots)
}

func countMaxDimensions(points []string) (int, int) {
  maxX := 0
  maxY := 0
  for _, point := range points {
    x, y := pointToCoordinates(point)

    if (x > maxX) { maxX = x }
    if (y > maxY) { maxY = y }
  }

  return maxX, maxY
}

func pointToCoordinates(point string) (int, int) {
  xy := strings.Split(point, ",")
  x, _ := strconv.Atoi(xy[0])
  y, _ := strconv.Atoi(xy[1])

  return x, y
}

func printDots(dots Dots) {
  maxX := len(dots)
  maxY := len(dots[0])

  for y := 0; y < maxY; y++ {
    for x := 0; x < maxX; x++ {
      if (dots[x][y] == 1) {
        fmt.Print("#")
      } else {
        fmt.Print(".")
      }
    }
    fmt.Println("")
  }
}
