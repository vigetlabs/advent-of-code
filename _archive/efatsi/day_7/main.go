package main

import (
  "fmt"
  "os"
  "strings"
  "strconv"
  "math"
  "sort"
)

// const filename = "example.txt"
const filename = "input.txt"

func main() {
  data, _ := os.ReadFile(filename)

  trimmedData := strings.Trim(string(data), "\n ")
  positionStrings := strings.Split(trimmedData, ",")
  positions := strToInt(positionStrings)

  // Part 1
  sort.Ints(positions)
  median := positions[len(positions) / 2]
  requiredFuel := calculateFuel(positions, median)
  fmt.Println("Fuel: ", requiredFuel)

  // Part 2
  average := calculateAverage(positions)
  expensiveFuel := calculateExpensiveFuel(positions, average)
  fmt.Println("Expensive Fuel: ", expensiveFuel)
}

func calculateFuel(positions []int, target int) int {
  fuel := 0

  for _, x := range positions {
    fuel += int(math.Abs(float64(x - target)))
  }

  return fuel
}

func calculateExpensiveFuel(positions []int, target int) int {
  fuel := 0

  for _, x := range positions {
    distance := int(math.Abs(float64(x - target)))
    fuel += nthTriangle(distance)
  }

  return fuel
}

func nthTriangle(input int) int {
  // input + (input - 1) + (input - 2) + ... + 0
  return ((input * input) + input) / 2
}

func calculateAverage(ints []int) int {
  sum := 0

  for _, x := range ints {
    sum += x
  }

  return sum / len(ints)
}

func strToInt(strings []string) []int {
  ints := make([]int, len(strings))

  for i, str := range strings {
    ints[i], _ = strconv.Atoi(str)
  }

  return ints
}
