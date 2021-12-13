package main

import (
  "fmt"
  "os"
  "strings"
  "strconv"
)

type Dots [][]byte

// const debug = true
// const filename = "example.txt"

const debug = false
const filename = "input.txt"

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
  for x := 0; x <= maxX; x++ {
    column := make([]byte, maxY + 1)
    dots = append(dots, column)
  }

  for _, point := range points {
    x, y := pointToCoordinates(point)
    dots[x][y] = 1
  }

  if debug {
    fmt.Println("maxX", maxX)
    fmt.Println("maxY", maxY)
    fmt.Println("instructions", instructions)

    printDots(dots)
  }


  // foldY(&dots, 7)
  //
  // if debug {
  //   printDots(dots)
  // }

  foldX(&dots, 655)

  if debug {
    printDots(dots)
  }

  dotCount := countDots(dots)
  fmt.Println("dotCount:", dotCount)

  // TODO:
  // - parse fold instructions
}

func countDots(dots Dots) int {
  maxX, maxY := dimensionsOf(dots)
  fmt.Println("maxX, maxY: ", maxX, maxY)

  count := 0
  for x := 0; x < maxX; x++ {
    for y := 0; y < maxY; y++ {
      if dots[x][y] == 1 { count++ }
    }
  }

  return count
}

func foldY(dotsPtr *Dots, foldLine int) {
  dots := *dotsPtr
  maxX, maxY := dimensionsOf(dots)

  for x := 0; x < maxX; x++ {
    for y := foldLine + 1; y < maxY; y++ {
      if dots[x][y] == 1 {
        dots[x][foldLine - (y - foldLine)] = 1
      }
    }

    dots[x] = dots[x][:foldLine]
  }
}

func foldX(dotsPtr *Dots, foldLine int) {
  dots := *dotsPtr
  maxX, maxY := dimensionsOf(dots)

  for x := foldLine + 1; x < maxX; x++ {
    for y := 0; y < maxY; y++ {
      if dots[x][y] == 1 {
        dots[foldLine - (x - foldLine)][y] = 1
      }
    }
  }

  *dotsPtr = dots[:foldLine]
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
  fmt.Println("")
  maxX, maxY := dimensionsOf(dots)

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

func dimensionsOf(dots Dots) (int, int) {
  maxX := len(dots)
  maxY := len(dots[0])

  return maxX, maxY
}
