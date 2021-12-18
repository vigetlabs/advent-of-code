package main

import (
  "fmt"
  "math"
)

const debug = false

// Skip the parsing today
// Example:
// target area: x=20..30, y=-10..-5
// const minX = 20
// const maxX = 30
// const minY = -10
// const maxY = -5

// Input:
// target area: x=88..125, y=-157..-103
const minX = 88
const maxX = 125
const minY = -157
const maxY = -103

const xLength = maxX - minX + 1
const yLength = maxY - minY + 1

var infiniteXs = findInfinites()

type Shot struct {
  xVelocity int
  yVelocity int
  xTarget int
  yTarget int
}

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

  validShots := make([]Shot, 0)

  for step := 1; step <= maxSteps; step++ {
    for _, shot := range shotsWithStep(step) {
      if !alreadyTracked(validShots, shot) {
        validShots = append(validShots, shot)
      }
    }
  }

  fmt.Println("Part 2:", len(validShots))
}

func shotsWithStep(step int) []Shot {
  if debug { fmt.Println("Steps:", step) }

  yTargets := make([]int, 0)
  xTargets := make([]int, 0)

  if step == 1 {

    // aim, fire
    for y := minY; y <= maxY; y++ {
      yTargets = append(yTargets, y)
    }

    for x := minX; x <= maxX; x++ {
      xTargets = append(xTargets, x)
    }

  } else if step % 2 == 0 {

    // even steps (eg: 5+4+3+2=14 -- 4 steps puts you on positions in between multiples of 4)
    for y := minY; y <= maxY; y++ {
      if (y + (step/2)) % step == 0 {
        yTargets = append(yTargets, y)
      }
    }

    for x := minX; x <= maxX; x++ {
      // x >= addToZero(step-1)) -- ensure x is only matching on positive sets
      // infiniteColsLessThan are the lines that are possible to drop down on w/ less steps than available
      if ((x + (step/2)) % step == 0 && x >= addToZero(step-1)) || contains(infiniteColsLessThan(step), x) {
        xTargets = append(xTargets, x)
      }
    }

  } else {

    // odd steps (eg: 11+10+9=30 -- 3 steps puts you on positions that are multiples of 3)
    for y := minY; y <= maxY; y++ {
      if y % step == 0 {
        yTargets = append(yTargets, y)
      }
    }

    for x := minX; x <= maxX; x++ {
        // x >= addToZero(step-1)) -- ensure x is only matching on positive sets
      // infiniteColsLessThan are the lines that are possible to drop down on w/ less steps than available
      if (x % step == 0 && x >= addToZero(step-1)) || contains(infiniteColsLessThan(step), x) {
        xTargets = append(xTargets, x)
      }
    }
  }

  shots := assembleShots(xTargets, yTargets, step)

  if debug {
    if (len(shots) > 0) {
      fmt.Println("Shots:")
      for _, shot := range shots {
        printShot(shot)
      }
    }

    fmt.Println("Count:")
    fmt.Println("  ", len(shots))
    fmt.Println("")
  }

  return shots
}

func infiniteColsLessThan(step int) []int {
  infiniteCols := make([]int, 0)

  for xVelocity, _ := range infiniteXs {
    if xVelocity < step {
      infiniteCols = append(infiniteCols, addToZero(xVelocity))
    }
  }

  return infiniteCols
}

func assembleShots(xTargets []int, yTargets []int, step int) []Shot {
  shots := make([]Shot, 0)

  for _, xTarget := range xTargets {
    for _, yTarget := range yTargets {
      xVelocity := getXVelocity(xTarget, step)
      yVelocity := getYVelocity(yTarget, step)

      shots = append(shots, Shot{xVelocity, yVelocity, xTarget, yTarget})
    }
  }

  return shots
}

func getYVelocity(yTarget int, step int) int {
  if step == 1 { return yTarget }

  return step + getOffset(yTarget, step)
}

func getXVelocity(xTarget int, step int) int {
  if step == 1 { return xTarget }

  for velocity, target := range infiniteXs {
    if (step >= velocity && target == xTarget) { return velocity }
  }

  return step + getOffset(xTarget, step)
}

func getOffset(target int, step int) int {
  return (target - addToZero(step)) / step
}

func findInfinites() map[int]int {
  low := int(math.Ceil(solveQuadratic(minX)))
  high := int(math.Floor(solveQuadratic(maxX)))

  // infinite infinite infinite infinite infinite infinite infinite infinite infinite infinite infinite infinite infinite infinite infinite infinite infinite infinite infinite infinite infinite infinite infinite infinite infinite infinite infinite infinite infinite infinite infinite infinite infinite infinite infinite infinite infinite infinite infinite infinite infinite infinite
  infinites := make(map[int]int)
  for velocity := low; velocity <= high; velocity++ {
    infinites[velocity] = addToZero(velocity)
  }

  if debug {
    fmt.Println("Infinites....")
    fmt.Println("low: ", low)
    fmt.Println("high:", high)
    fmt.Println("addToZero(low): ", addToZero(low))
    fmt.Println("addToZero(high):", addToZero(high))
    fmt.Println("infinites", infinites)
    fmt.Println("")
  }

  return infinites
}

// -- helpers --

func addToZero(input int) int {
  // input + input-1 + input-2 + ... + 0
  return input * (input + 1) / 2
}

// oddly enough, solveQuadratic is the inverse of addToZero
func solveQuadratic(c int) float64 {
  return (math.Sqrt(float64(8 * c) + 1) - 1)/ 2
}

func contains(slice []int, value int) bool {
  for _, v := range slice {
    if v == value {
      return true
    }
  }

  return false
}

func alreadyTracked(slice []Shot, shot Shot) bool {
  for _, v := range slice {
    if (v.xVelocity == shot.xVelocity) && (v.yVelocity == shot.yVelocity) {
      return true
    }
  }

  return false
}

func printShot(shot Shot) {
  fmt.Printf("  Vel: %d,%d -- Dest: %d,%d", shot.xVelocity, shot.yVelocity, shot.xTarget, shot.yTarget)
  fmt.Println("")
}
