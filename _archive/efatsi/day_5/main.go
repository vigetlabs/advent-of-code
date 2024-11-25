package main

import (
  "fmt"
  "os"
  "strings"
  "strconv"
)

const debug = false
const dimension = 1000
const filePath = "input.txt"

// const debug = true
// const dimension = 10
// const filePath = "example.txt"

var coordinates [dimension][dimension]int

func main() {
  data, _ := os.ReadFile(filePath)

  trimmed_data := strings.Trim(string(data), "\n ")
  vent_inputs := strings.Split(trimmed_data, "\n")

  for _, input := range vent_inputs {
    vent_bounds := strings.Split(input, " -> ")
    start := vent_bounds[0]
    end := vent_bounds[1]
    recordVent(start, end)
  }

  countOverlaps()
}

func recordVent(start string, end string) {
  if debug { fmt.Println(start, " - ", end) }

  start_coordinates := strings.Split(start, ",")
  end_coordinates := strings.Split(end, ",")

  x1, _ := strconv.Atoi(start_coordinates[0])
  y1, _ := strconv.Atoi(start_coordinates[1])
  x2, _ := strconv.Atoi(end_coordinates[0])
  y2, _ := strconv.Atoi(end_coordinates[1])

  if x1 == x2 {
    min, max := getMinMax(y1, y2)
    for i := min; i <= max; i++ {
      coordinates[x1][i]++
    }
  } else if y1 == y2 {
    min, max := getMinMax(x1, x2)
    for i := min; i <= max; i++ {
      coordinates[i][y1]++
    }
  } else {
    // diagonal line
    // calculate directions
    xDirection := 1
    if x1 > x2 {
      xDirection = -1
    }

    yDirection := 1
    if y1 > y2 {
      yDirection = -1
    }

    // calculate steps to take
    minX, maxX := getMinMax(x1, x2)
    steps := maxX - minX

    if debug {
      fmt.Println("xDirection: ", xDirection)
      fmt.Println("yDirection: ", yDirection)
      fmt.Println("steps: ", steps)
    }

    for i := 0; i <= steps; i++ {
      x := x1 + (i * xDirection)
      y := y1 + (i * yDirection)
      coordinates[x][y]++
    }
  }

  if debug { printCoordinates() }
}

func countOverlaps() {
  sum := 0

  for x := 0; x < dimension; x++ {
    for y := 0; y < dimension; y++ {
      if coordinates[x][y] > 1 {
        sum++
      }
    }
  }

  fmt.Println("Overlaps: ", sum)
}

func getMinMax(a int, b int) (int, int) {
  if a < b {
    return a, b
  } else {
    return b, a
  }
}

func printCoordinates()  {
  for x := 0; x < dimension; x++ {
    for y := 0; y < dimension; y++ {
      fmt.Print(coordinates[y][x], " ")
    }
    fmt.Println("")
  }
  fmt.Println("")
  fmt.Println("")
}
