package main

import (
  "fmt"
  "os"
  "strings"
  "strconv"
)

// const debug = false
// const filename = "example.txt"
// const DIMENSION = 10

const debug = false
const filename = "input.txt"
const DIMENSION = 100

const BIG_DIMENSION = DIMENSION * 5

type Riskmap [][]Position

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

  solvePartOne(riskmap)
  solvePartTwo(riskmap)
}

func solvePartOne(riskmap Riskmap) {
  riskmap[0][0].lowestRisk = 0

  commenceFatsiAlgorithm(riskmap, DIMENSION)
}

func solvePartTwo(riskmap Riskmap) {
  bigRiskmap := loadBigRiskmap(riskmap)
  bigRiskmap[0][0].lowestRisk = 0

  commenceFatsiAlgorithm(bigRiskmap, BIG_DIMENSION)
}

func commenceFatsiAlgorithm(riskmap Riskmap, dimension int) {
  if debug {
    printRiskmap(riskmap)
  }

  var focus Position
  unexplored := make([]Position, 1)
  unexplored[0] = riskmap[0][0]

  for (len(unexplored) > 0) {
    focus = unexplored[0]
    unexplored = unexplored[1:]

    if debug {
      fmt.Println("Iterating")
      fmt.Println("focus     :", focus)
    }

    for _, validCoordinates := range focus.neighbors(dimension) {
      neighbor := &riskmap[validCoordinates[0]][validCoordinates[1]]

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

  fmt.Println("Shortest path:", riskmap[dimension - 1][dimension - 1].lowestRisk)
}

func modulo(number int) int {
  if number > 9 {
    return number - 9
  } else {
    return number
  }
}

func loadRiskmap(riskLines []string) Riskmap {
  var riskmap Riskmap

  for y, line := range riskLines {
    riskReadings := strings.Split(line, "")

    for x, reading := range riskReadings {
      if y == 0 {
        riskmap = append(riskmap, make([]Position, DIMENSION))
      }

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

func loadBigRiskmap(riskmap Riskmap) Riskmap {
  var bigRiskmap Riskmap

  for bigY := 0; bigY < 5; bigY++ {
    for bigX := 0; bigX < 5; bigX++ {
      multiplier := bigX + bigY

      for y := 0; y < DIMENSION; y++ {
        for x := 0; x < DIMENSION; x++ {
          if bigY == 0 && y == 0 {
            bigRiskmap = append(bigRiskmap, make([]Position, BIG_DIMENSION))
          }

          bigRiskmap[x + (bigX * DIMENSION)][y + (bigY * DIMENSION)] = Position {
            x: x + (bigX * DIMENSION),
            y: y + (bigY * DIMENSION),
            risk: modulo(riskmap[x][y].risk + multiplier),
            lowestRisk: BIG_DIMENSION * 2 * 10, // over max possible
          }
        }
      }
    }
  }

  return bigRiskmap
}

func printRiskmap(riskmap Riskmap) {
  dimension := len(riskmap)

  for y := 0; y < dimension; y++ {
    for x := 0; x < dimension; x++ {
      fmt.Print(riskmap[x][y].risk)
      fmt.Printf(" (%3d) ", riskmap[x][y].lowestRisk)
    }
    fmt.Println("")
  }

  fmt.Println("")
}

func (position *Position) neighbors(dimension int) [][2]int {
  neighbors := make([][2]int, 0)

  x := position.x
  y := position.y

  // Check Up
  if y > 0 {
    neighbors = append(neighbors, [2]int{x, y-1})
  }

  // Check Down
  if y < dimension - 1 {
    neighbors = append(neighbors, [2]int{x, y+1})
  }

  // Check Left
  if x > 0 {
    neighbors = append(neighbors, [2]int{x-1, y})
  }

  // Check Right
  if x < dimension - 1 {
    neighbors = append(neighbors, [2]int{x+1, y})
  }

  return neighbors
}
