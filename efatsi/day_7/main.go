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
  fmt.Println("Fuel: ", required_fuel)

  // Part 2
  average := calculate_average(positions)
  expensive_fuel := calculate_expensive_fuel(positions, int(average))
  fmt.Println("Expensive Fuel: ", expensive_fuel)
}

func calculate_average(ints []int) float64 {
  sum := 0

  for _, x := range ints {
    sum += x
  }

  return float64(sum) / float64(len(ints))
}

func calculate_fuel(positions []int, target int) int {
  fuel := 0

  for _, x := range positions {
    fuel += int(math.Abs(float64(x - target)))
  }

  return fuel
}

func calculate_expensive_fuel(positions []int, target int) int {
  fuel := 0

  for _, x := range positions {
    distance := int(math.Abs(float64(x - target)))
    fuel += nth_triangle(distance)
  }

  return fuel
}

func nth_triangle(input int) int {
  // input + (input - 1) + (input - 2) + ... + 0
  return ((input * input) + input) / 2
}

func str_to_int(strings []string) []int {
  ints := make([]int, len(strings))

  for i, str := range strings {
    ints[i], _ = strconv.Atoi(str)
  }

  return ints
}
