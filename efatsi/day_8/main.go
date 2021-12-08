package main

import (
  "fmt"
  "os"
  "strings"
)

// const filename = "example.txt"
const filename = "input.txt"

func main() {
  data, _ := os.ReadFile(filename)

  trimmedData := strings.Trim(string(data), "\n ")
  lines := strings.Split(trimmedData, "\n")
  readings := assembleReadings(lines)

  // Part 1
  easyValueCount := countEasyValues(readings)
  fmt.Println("easyValueCount: ", easyValueCount)
}

func countEasyValues(readings map[[10]string][4]string) int {
  sum := 0

  for _, value := range readings {
    for _, outputDigit := range value {
      if contains([]int{2, 3, 4, 7}, len(outputDigit)) {
        sum++
      }
    }
  }

  return sum
}

// Returns map with
// keys:   [10]string
// values: [4]string
func assembleReadings(lines []string) map[[10]string][4]string {
  readings := make(map[[10]string][4]string, 0)

  for _, line := range lines {
    lineData := strings.Split(line, " | ")
    inputSlice := strings.Split(lineData[0], " ")
    outputSlice := strings.Split(lineData[1], " ")

    var inputArray [10]string
    var outputArray [4]string
    copy(inputArray[:], inputSlice)
    copy(outputArray[:], outputSlice)

    readings[inputArray] = outputArray
  }

  return readings
}

func contains(arr []int, val int) bool {
  for _, v := range arr {
    if v == val {
      return true
    }
  }

  return false
}
