package main

import (
  "fmt"
)

const debug = true

// Skip the parsing today
// Example:
// target area: x=20..30, y=-10..-5
const minX = 20
const maxX = 30
const minY = -10
const maxY = -5

// Input:
// target area: x=88..125, y=-157..-103
// const minX = 88
// const maxX = 125
// const minY = -157
// const maxY = -103

const xLength = maxX - minX + 1
const yLength = maxY - minY + 1

func main() {
  solvePartOne()
  solvePartTwo()
}

func solvePartOne() {
  maxYVelocity := (minY * -1) - 1
  maxHeight := addToZero(maxYVelocity)

  fmt.Println("Part 1:", maxHeight)
}

func solvePartTwo() {
  maxYVelocity := (minY * -1) - 1
  maxSteps := (maxYVelocity + 1) * 2

  shotCount := 0

  for step := 1; step <= maxSteps; step++ {
    shotCount += shotsWithStep(step)
  }

  fmt.Println("shotCount:", shotCount)
}

func shotsWithStep(step int) int {
  if debug { fmt.Println("Count for shot:", step) }

  // aim, fire
  if step == 1 {
    if debug {
      fmt.Println("Count:", xLength, "*", yLength, "=", xLength * yLength)
      fmt.Println("")
    }
    return xLength * yLength
  }

  // even steps (eg: 5+4+3+2=14 -- 4 steps puts you on positions in between multiples of 4)
  if step % 2 == 0 {
    yCount := 0
    xCount := 0

    for y := minY; y <= maxY; y++ {
      if (y + (step/2)) % step == 0 {
        if debug { fmt.Println("Adding y", y) }
        yCount++
      }
    }

    for x := minX; x <= maxX; x++ {
      // x >= addToZero(step-1)) -- ensure x is only matching on positive sets
      // infiniteColsLessThan are the lines that are possible to drop down on w/ less steps than available
      if ((x + (step/2)) % step == 0 && x >= addToZero(step-1)) || contains(infiniteColsLessThan(step), x) {
        if debug { fmt.Println("Adding x", x) }
        xCount++
      }
    }

    if debug {
      fmt.Println("Count:", yCount, "*", xCount, "=", yCount * xCount)
      fmt.Println("")
    }
    return yCount * xCount
  }

  // odd steps (eg: 11+10+9=30 -- 3 steps puts you on positions that are multiples of 3)
  if step % 2 == 1 {
    yCount := 0
    xCount := 0

    for y := minY; y <= maxY; y++ {
      if y % step == 0 {
        if debug { fmt.Println("Adding y", y) }
        yCount++
      }
    }

    for x := minX; x <= maxX; x++ {
      // x >= addToZero(step-1)) -- ensure x is only matching on positive sets
      // infiniteColsLessThan are the lines that are possible to drop down on w/ less steps than available
      if (x % step == 0 && x >= addToZero(step-1)) || contains(infiniteColsLessThan(step), x) {
        if debug { fmt.Println("Adding x", x) }
        xCount++
      }
    }

    if debug {
      fmt.Println("Count:", yCount, "*", xCount, "=", yCount * xCount)
      fmt.Println("")
    }
    return yCount * xCount
  }

  fmt.Println("Shouldn't get here")
  return 0
}

func infiniteColsLessThan(step int) []int {
  infiniteXs := []int{6,7}
  infiniteCols := make([]int, 0)

  for _, xVelocity := range infiniteXs {
    if xVelocity < step {
      infiniteCols = append(infiniteCols, addToZero(xVelocity))
    }
  }

  return infiniteCols
}

func addToZero(input int) int {
  // input + input-1 + input-2 + ... + 0
  return input * (input + 1) / 2
}

func contains(slice []int, value int) bool {
  for _, v := range slice {
    if v == value {
      return true
    }
  }

  return false
}
