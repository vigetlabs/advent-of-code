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

  trimmed_data := strings.Trim(string(data), "\n ")
  position_strings := strings.Split(trimmed_data, ",")
  positions := str_to_int(position_strings)

  // Part 1
  sort.Ints(positions)
  median := positions[len(positions) / 2]
  required_fuel := calculate_fuel(positions, median)
  fmt.Println("Target: ", median)
  fmt.Println("Fuel:   ", required_fuel)
}

func calculate_fuel(positions []int, target int) int {
  fuel := 0

  for _, x := range positions {
    fuel += int(math.Abs(float64(x - target)))
  }

  return fuel
}

func str_to_int(strings []string) []int {
  ints := make([]int, len(strings))

  for i, str := range strings {
    ints[i], _ = strconv.Atoi(str)
  }

  return ints
}
