package main

import (
  "fmt"
  "math"
)

const debug = true

// Guesses
// 112 / 3482: That's not the right answer; your answer is too low
// - whoops only building array of one infinite
// 112 / 3661/3665/3669: That's not the right answer; your answer is too high
// - building all -1 infinites
//   - a little dumb, but it makes example data happy so i got excited
// 119 / 3852:
// - building all the infinites
//   - must be a bug in the way infinites are being counted


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

var infiniteXs = findInfinites()

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

  validShots := make([][2]int, 0)

  for step := 1; step <= maxSteps; step++ {
    validShots = append(validShots, shotsWithStep(step)...)
  }

  if debug {
    fmt.Println("ALL PAIRS:")
    for _, pair := range validShots {
      fmt.Printf("  %d,%d", pair[0], pair[1])
      fmt.Println("")
    }
    fmt.Println("")
  }

  fmt.Println("shotCount:", len(validShots))
}

func shotsWithStep(step int) [][2]int {
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

  pairs := assemblePairs(xTargets, yTargets)

  if debug {
    if (len(pairs) > 0) {
      fmt.Println("Pairs:")
      for _, pair := range pairs {

        fmt.Printf("  %d,%d", pair[0], pair[1])
        fmt.Println("")
      }
    }

    fmt.Println("Count:")
    fmt.Println("  ", len(yTargets) * len(xTargets))
    fmt.Println("")
  }

  return pairs
}

func assemblePairs(xTargets []int, yTargets []int) [][2]int {
  pairs := make([][2]int, 0)

  for _, y := range yTargets {
    for _, x := range xTargets {
      pairs = append(pairs, [2]int{x, y})
    }
  }

  return pairs
}

func infiniteColsLessThan(step int) []int {
  infiniteCols := make([]int, 0)

  for _, xVelocity := range infiniteXs {
    if xVelocity < step {
      infiniteCols = append(infiniteCols, addToZero(xVelocity))
    }
  }

  return infiniteCols
}

func findInfinites() []int {
  low := int(math.Ceil(solveQuadratic(minX)))
  high := int(math.Floor(solveQuadratic(maxX)))

  // infinite infinite infinite infinite infinite infinite infinite infinite infinite infinite infinite infinite infinite infinite infinite infinite infinite infinite infinite infinite infinite infinite infinite infinite infinite infinite infinite infinite infinite infinite infinite infinite infinite infinite infinite infinite infinite infinite infinite infinite infinite infinite
  infinites := make([]int, 0)
  for infinite := low; infinite <= high; infinite++ {
    infinites = append(infinites, infinite)
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
