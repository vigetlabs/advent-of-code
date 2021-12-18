package main

import (
  "fmt"
  "math"

  "day_17/targeter"
  "day_17/velocitier"
)

const debug = true

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
      if debug {
        printShot(shot)
      }

      if !alreadyTracked(validShots, shot) {
        validShots = append(validShots, shot)
      }
    }
  }

  fmt.Println("Part 2:", len(validShots))
}

func shotsWithStep(step int) []Shot {
  if debug { fmt.Println("Steps:", step) }

  t := targeter.New(minX, maxX, minY, maxY, infiniteXs)
  yTargets := t.GetYTargets(step)
  xTargets := t.GetXTargets(step)

  return assembleShots(xTargets, yTargets, step)
}

func assembleShots(xTargets []int, yTargets []int, step int) []Shot {
  shots := make([]Shot, 0)

  v := velocitier.New(infiniteXs)

  for _, xTarget := range xTargets {
    for _, yTarget := range yTargets {
      xVelocity := v.GetXVelocity(xTarget, step)
      yVelocity := v.GetYVelocity(yTarget, step)

      shots = append(shots, Shot{xVelocity, yVelocity, xTarget, yTarget})
    }
  }

  return shots
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
