package main

import (
  "fmt"
  "os"
  "strings"
  "strconv"
)

const debug = true
const filename = "example.txt"
const DIMENSION = 10

// const debug = false
// const filename = "input.txt"
// const DIMENSION = 100

type Riskmap [DIMENSION][DIMENSION]Position

type Position struct {
  x int
  y int
  risk int
  lowestRisk int
  from *Position
}

func main() {
  data, _ := os.ReadFile(filename)

  trimmedData := strings.Trim(string(data), "\n ")
  riskLines := strings.Split(trimmedData, "\n")

  riskmap := loadRiskmap(riskLines)
  riskmap[0][0].lowestRisk = 0

  if debug {
    printRiskmap(riskmap)
  }

  var focus Position
  unexplored := make([]Position, 1)
  unexplored[0] = riskmap[0][0]

  for i := 0; i < 5; i++ {
  // for (len(unexplored) > 0) {
    focus = unexplored[0]
    unexplored = unexplored[1:]

    if debug {
      fmt.Println("Iterating")
      fmt.Println("focus     :", focus)
    }

    for _, neighborReference := range focus.neighbors(riskmap) {
      neighbor := &riskmap[neighborReference.x][neighborReference.y]

      if (focus.lowestRisk + neighbor.risk < neighbor.lowestRisk) {
        neighbor.from = &focus
        neighbor.lowestRisk = focus.lowestRisk + neighbor.risk

        unexplored = append(unexplored, *neighbor)
      }
    }

    if debug {
      fmt.Println("unexplored:", unexplored)
      printRiskmap(riskmap)
    }

  }

  printRiskmap(riskmap)

  // fmt.Println("bottom-right: ", riskmap[DIMENSION-1][DIMENSION-1])
  // inspect map[9][9]
}

func loadRiskmap(riskLines []string) Riskmap {
  var riskmap Riskmap

  for y, line := range riskLines {
    riskReadings := strings.Split(line, "")

    for x, reading := range riskReadings {
      risk, _ := strconv.Atoi(reading)
      riskmap[x][y] = Position {
        x: x,
        y: y,
        risk: risk,
        lowestRisk: DIMENSION * 2 * 10, // over max possible
      }
    }
  }

  return riskmap
}

func printRiskmap(riskmap Riskmap) {
  for y := 0; y < DIMENSION; y++ {
    for x := 0; x < DIMENSION; x++ {
      fmt.Print(riskmap[x][y].risk)
      fmt.Printf(" (%3d) ", riskmap[x][y].lowestRisk)
    }
    fmt.Println("")
  }

  fmt.Println("")
}

func (position *Position) neighbors(riskmap Riskmap) []Position {
  neighbors := make([]Position, 0)

  x := position.x
  y := position.y

  // Check Up
  if y > 0 {
    neighbors = append(neighbors, riskmap[x][y-1])
  }

  // Check Down
  if y < DIMENSION - 1 {
    neighbors = append(neighbors, riskmap[x][y+1])
  }

  // Check Left
  if x > 0 {
    neighbors = append(neighbors, riskmap[x-1][y])
  }

  // Check Right
  if x < DIMENSION - 1 {
    neighbors = append(neighbors, riskmap[x+1][y])
  }

  return neighbors
}
