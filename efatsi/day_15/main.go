package main

import (
  "fmt"
  "os"
  "strings"
  "strconv"
)

// const debug = true
// const filename = "example.txt"
// const DIMENSION = 10

// const debug = false
// const filename = "example_md.txt"
// const DIMENSION = 50

const debug = false
const filename = "input.txt"
const DIMENSION = 100

const BIG_DIMENSION = DIMENSION * 5

type Riskmap [DIMENSION][DIMENSION]Position
type BigRiskmap [BIG_DIMENSION][BIG_DIMENSION]Position

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

  // solvePartOne(riskmap)
  solvePartTwo(riskmap)
}

func solvePartOne(riskmap Riskmap) {
  riskmap[0][0].lowestRisk = 0

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

    for _, validCoordinates := range focus.neighbors(DIMENSION) {
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

  printRiskmap(riskmap)
}

func solvePartTwo(riskmap Riskmap) {
  bigRiskmap := loadBigRiskmap(riskmap)
  bigRiskmap[0][0].lowestRisk = 0

  if (debug) {
    printBigRiskmap(bigRiskmap)
  }

  var focus Position
  unexplored := make([]Position, 1)
  unexplored[0] = bigRiskmap[0][0]

  for (len(unexplored) > 0) {
    focus = unexplored[0]
    unexplored = unexplored[1:]

    if debug {
      if focus.x == 9 && focus.y == 1 {
        fmt.Println("Iterating")
        fmt.Println("focus     :", focus)

        fmt.Println("neighbors :", focus.neighbors(BIG_DIMENSION))
      }
    }

    for _, validCoordinates := range focus.neighbors(BIG_DIMENSION) {
      neighbor := &bigRiskmap[validCoordinates[0]][validCoordinates[1]]

      if debug {
        if focus.x == 9 && focus.y == 1 {
          fmt.Println("neighbor:", neighbor)
        }
      }

      if (focus.lowestRisk + neighbor.risk < neighbor.lowestRisk) {
        neighbor.from = &focus
        neighbor.lowestRisk = focus.lowestRisk + neighbor.risk

        unexplored = append(unexplored, *neighbor)
      }
    }

    if debug {
      fmt.Println("unexplored:", unexplored)
      printBigRiskmap(bigRiskmap)
    }

  }

  fmt.Println("Bottom right:", bigRiskmap[BIG_DIMENSION-1][BIG_DIMENSION-1])
  // printBigRiskmap(bigRiskmap)
}

func modulo(number int) int {
  if number > 9 {
    return number - 9
  } else {
    return number
  }
}

func loadBigRiskmap(riskmap Riskmap) BigRiskmap {
  var bigRiskmap BigRiskmap

  for bigX := 0; bigX < 5; bigX++ {
    for bigY := 0; bigY < 5; bigY++ {
      multiplier := bigX + bigY

      for y := 0; y < DIMENSION; y++ {
        for x := 0; x < DIMENSION; x++ {
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

func printBigRiskmap(riskmap BigRiskmap) {
  for y := 0; y < BIG_DIMENSION; y++ {
    for x := 0; x < BIG_DIMENSION; x++ {
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
