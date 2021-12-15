package main

import (
  "fmt"
  "os"
  "strings"
  "strconv"
)

const debug = false
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

  printRiskmap(riskmap)

  var focus Position
  currentRisk := 0
  unexplored := make([]Position, 1)
  unexplored[0] = riskmap[0][0]

  if debug {
    fmt.Println("currentRisk", currentRisk)
    fmt.Println("unexplored", unexplored)
    fmt.Println("")
  }

  for i := 0; i < 10; i++ {
    focus = unexplored[0]
    unexplored = unexplored[1:]

    if debug {
      fmt.Println("Iterating")
      fmt.Println("unexplored:", unexplored)
      fmt.Println("focus     :", focus)
    }

    neighbors := focus.neighbors(riskmap)
    for i := 0; i < len(neighbors); i++ {
      neighbor := neighbors[i]

      if (neighbor.risk + currentRisk < neighbor.lowestRisk) {
        neighbor.from = &focus
        neighbor.lowestRisk = neighbor.risk + currentRisk

        for _, nextNeighbor := range neighbor.neighbors(riskmap) {
          if nextNeighbor != focus {
            unexplored = append(unexplored, nextNeighbor)
          }
        }
      }
    }

    if debug {
      fmt.Println("After Scan")
      fmt.Println("unexplored:", unexplored)
      fmt.Println("")
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
    neighbors = append(neighbors, riskmap[y-1][x])
  }

  // Check Down
  if y < DIMENSION - 1 {
    neighbors = append(neighbors, riskmap[y+1][x])
  }

  // Check Left
  if x > 0 {
    neighbors = append(neighbors, riskmap[y][x-1])
  }

  // Check Right
  if x < DIMENSION - 1 {
    neighbors = append(neighbors, riskmap[y][x+1])
  }

  return neighbors
}
