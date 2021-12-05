package main

import (
  "fmt"
  "os"
  "strings"
  "strconv"
)

const dimension = 1000
var coordinates [dimension][dimension]int

func main() {
  // data, _ := os.ReadFile("example.txt")
  data, _ := os.ReadFile("input.txt")

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
  start_coordinates := strings.Split(start, ",")
  end_coordinates := strings.Split(end, ",")

  x1, _ := strconv.Atoi(start_coordinates[0])
  y1, _ := strconv.Atoi(start_coordinates[1])
  x2, _ := strconv.Atoi(end_coordinates[0])
  y2, _ := strconv.Atoi(end_coordinates[1])

  // only record vent if straight line: x1==x2 or y1==y2
  if x1 != x2 && y1 != y2 {
    return
  }

  if x1 == x2 {
    min, max := getMinMax(y1, y2)
    for i := min; i <= max; i++ {
      coordinates[x1][i]++
    }
  }

  if y1 == y2 {
    min, max := getMinMax(x1, x2)
    for i := min; i <= max; i++ {
      coordinates[i][y1]++
    }
  }

  // printCoordinates()
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
