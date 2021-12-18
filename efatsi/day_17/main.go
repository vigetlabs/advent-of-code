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

  yTargets := getYTargets(step)
  xTargets := getXTargets(step)

  shots := assembleShots(xTargets, yTargets, step)

  if debug {
    if (len(shots) > 0) {
      fmt.Println("Shots:")
      for _, shot := range shots {
        printShot(shot)
      }
    }
    fmt.Println("")
  }

  return shots
}

func getYTargets(step int) []int {
  yTargets := make([]int, 0)

  for y := minY; y <= maxY; y++ {
    if validYStep(y, step) {
      yTargets = append(yTargets, y)
    }
  }

  return yTargets
}

func getXTargets(step int) []int {
  xTargets := make([]int, 0)

  for x := minX; x <= maxX; x++ {
    if validXStep(x, step) {
      xTargets = append(xTargets, x)
    }
  }

  return xTargets
}

func validYStep(y int, step int) bool {
  // Nothing fancy about y steps except if the modulo matches
  return moduloMatch(y, step)
}

func validXStep(x int, step int) bool {
  // x is a little trickier:
  // - check if modulo matches AND it's even possible for x to get there (since x velocity can never be negative)
  // - OR check if we're on an infinite dropping column

  return (moduloMatch(x, step) && x >= addToZero(step)) || contains(infiniteColsLessThan(step), x)
}

func moduloMatch(val int, step int) bool {
  if step % 2 == 0 {
    // even steps (eg: 5+4+3+2=14 -- 4 steps puts you on positions in between multiples of 4)
    return (val + (step/2)) % step == 0
  } else {
    // odd steps (eg: 11+10+9=30 -- 3 steps puts you on positions that are multiples of 3)
    return val % step == 0
  }
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
  return step + getOffset(yTarget, step)
}

func getXVelocity(xTarget int, step int) int {
  for velocity, target := range infiniteXs {
    if (step >= velocity && target == xTarget) { return velocity }
  }

  return step + getOffset(xTarget, step)
}

func getOffset(target int, step int) int {
  // eg: if heading towards -9 and it takes 2 steps
  // - steps would be [-4, -5]
  // - offset is calculatable by starting with sum([2, 1]) (`addToZero(step)`) and
  //   figuring out how much each individual element needs to adjest to add up to `target`
  // - in this case: (-9 - 3) / 2 = -6
  //
  // => so the first step of a downward trending array w/ len == steps and sum == target:
  //    2 + -6 == -4
  return (target - addToZero(step)) / step
}

func findInfinites() map[int]int {
  low := int(math.Ceil(solveQuadratic(minX)))
  high := int(math.Floor(solveQuadratic(maxX)))

  infinites := make(map[int]int)
  for velocity := low; velocity <= high; velocity++ {
    infinites[velocity] = addToZero(velocity)
  }

  if debug {
    fmt.Println("Infinites....")
    fmt.Println(infinites)
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
  fmt.Printf("  Vel: %3d,%3d -- Dest: %3d,%3d", shot.xVelocity, shot.yVelocity, shot.xTarget, shot.yTarget)
  fmt.Println("")
}
