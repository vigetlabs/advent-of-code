package main

import (
  "fmt"
  "os"
  "strings"
  "strconv"
)

// const readings_count = 1000
// const reading_length = 12
const readings_count = 12
const reading_length = 5

func main() {
  // data, err := os.ReadFile("input.txt")
  data, err := os.ReadFile("example.txt")
  if err != nil {
    panic(err)
  }
  trimmed_data := strings.Trim(string(data), "\n ")
  readings := strings.Split(trimmed_data, "\n")

  positioned_bits := assemblePositionedBits(readings)

  solvePartOne(positioned_bits)
}

func solvePartOne(positioned_bits [][readings_count]int) {
  // Part 1 solution
  most_common_bits, least_common_bits := getMostCommon(positioned_bits)
  most_common_bit_string := arrayToString(most_common_bits)
  least_common_bit_string := arrayToString(least_common_bits)

  gamma, err := strconv.ParseInt(most_common_bit_string, 2, 64)
  epsilon, err := strconv.ParseInt(least_common_bit_string, 2, 64)
  check(err)

  fmt.Println(gamma * epsilon)
}

func assemblePositionedBits(readings []string) [][readings_count]int {
  // Assemble data into a slice of arrays of positioned bits
  // Slice: useful for making functions that operate on the dataset able to take
  //   slices of dynamic length
  positioned_bits := make([][readings_count]int, reading_length)

  for i := 0; i < readings_count; i++ {
    for j := 0; j < reading_length; j++ {
      positioned_bits[j][i], _ = strconv.Atoi(readings[i][j:j+1])
    }
  }

  return positioned_bits
}

func getMostCommon(positioned_bits [][readings_count]int) ([]int, []int) {
  most_common_bits := make([]int, len(positioned_bits))
  least_common_bits := make([]int, len(positioned_bits))

  for j := 0; j < len(positioned_bits); j++ {
    sum := 0
    for i := 0; i < readings_count; i++ {
      sum += positioned_bits[j][i]
    }

    if sum >= (readings_count / 2) {
      most_common_bits[j] = 1
      least_common_bits[j] = 0
    } else {
      most_common_bits[j] = 0
      least_common_bits[j] = 1
    }
  }

  return most_common_bits, least_common_bits
}

func arrayToString(slice []int) string {
  to_return := ""

  for i := 0; i < len(slice); i++ {
    to_return += strconv.Itoa(slice[i])
  }

  return to_return
}

func parseReading(bit string) int {
  if (bit == "1") {
    return 1
  } else {
    return 0
  }
}

func check(e error) {
  if e != nil {
    panic(e)
  }
}
