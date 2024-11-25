package main

import (
  "fmt"
  "os"
  "strings"
  "strconv"
)

func main() {
  data, err := os.ReadFile("input.txt")
  check(err)

  trimmed_data := strings.Trim(string(data), "\n ")
  readings := strings.Split(string(trimmed_data), "\n")

  // First challenge
  readingInts := intsFromStrings(readings)
  fmt.Println(countIncreases(readingInts))

  // Second challenge
  readingWindows := getWindows(readingInts)
  fmt.Println(countIncreases(readingWindows))
}

func intsFromStrings(readings []string) []int {
  intSlice := make([]int, 0)

  for i := 0; i < len(readings); i++ {
    val, err := strconv.Atoi(readings[i])
    check(err)

    intSlice = append(intSlice, val)
  }

  return intSlice
}

func getWindows(readingInts []int) []int {
  windows := make([]int, 0)

  for i := 0; i < len(readingInts) - 2; i++ {
    sum := readingInts[i] + readingInts[i + 1] + readingInts[i + 2]
    windows = append(windows, sum)
  }

  return windows
}

func check(e error) {
  if e != nil {
    panic(e)
  }
}

func countIncreases(readings []int) int {
  increases := 0
  for i := 1; i < len(readings); i++ {
    last := readings[i - 1]
    current := readings[i]

    if (current > last) {
      increases++
    }
  }

  return increases
}
