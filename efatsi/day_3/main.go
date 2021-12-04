package main

import (
  "fmt"
  "os"
  "strings"
  "strconv"
)

// const reading_length = 5
const reading_length = 12

func main() {
  // data, err := os.ReadFile("example.txt")
  data, err := os.ReadFile("input.txt")
  check(err)

  trimmed_data := strings.Trim(string(data), "\n ")
  readings := strings.Split(trimmed_data, "\n")

  solvePartOne(readings)
  solvePartTwo(readings)
}

func solvePartOne(readings []string) {
  positioned_bits := assemblePositionedBits(readings)
  most_common_bits, least_common_bits := getMostAndLeastCommonBits(positioned_bits)

  most_common_bit_string := arrayToString(most_common_bits)
  least_common_bit_string := arrayToString(least_common_bits)

  gamma, err := strconv.ParseInt(most_common_bit_string, 2, 64)
  epsilon, err := strconv.ParseInt(least_common_bit_string, 2, 64)
  check(err)

  fmt.Println(gamma * epsilon)
}

func solvePartTwo(readings []string) {
  oxygen_reading := filterReadingsBy(readings, getMostCommonBits, 0)
  co2_reading := filterReadingsBy(readings, getLeastCommonBits, 0)

  oxygen, err := strconv.ParseInt(oxygen_reading, 2, 64)
  co2, err := strconv.ParseInt(co2_reading, 2, 64)
  check(err)

  fmt.Println(oxygen * co2)
}

func filterReadingsBy(readings []string, filter_method func(positioned_bits [][]int) []int, index int) string {
  positioned_bits := assemblePositionedBits(readings)
  filtered_bits := filter_method(positioned_bits)
  filtered_bit := strconv.Itoa(filtered_bits[index])

  filtered_set := make([]string, 0)
  for _, reading := range readings {
    if (reading[index:index+1] == filtered_bit) {
      filtered_set = append(filtered_set, reading)
    }
  }

  if len(filtered_set) == 1 {
    return filtered_set[0]
  } else {
    return filterReadingsBy(filtered_set, filter_method, index + 1)
  }
}

func assemblePositionedBits(readings []string) [][]int {
  positioned_bits := make([][]int, reading_length)

  for j := 0; j < reading_length; j++ {
    positioned_bits[j] = make([]int, len(readings))

    for i := 0; i < len(readings); i++ {
      positioned_bits[j][i], _ = strconv.Atoi(readings[i][j:j+1])
    }
  }

  return positioned_bits
}

func getMostCommonBits(positioned_bits [][]int) []int {
  most_common, _ := getMostAndLeastCommonBits(positioned_bits)
  return most_common
}

func getLeastCommonBits(positioned_bits [][]int) []int {
  _, least_common := getMostAndLeastCommonBits(positioned_bits)
  return least_common
}

func getMostAndLeastCommonBits(positioned_bits [][]int) ([]int, []int) {
  readings_count := len(positioned_bits[0])

  most_common_bits := make([]int, len(positioned_bits))
  least_common_bits := make([]int, len(positioned_bits))

  for j := 0; j < len(positioned_bits); j++ {
    sum := 0
    for i := 0; i < readings_count; i++ {
      sum += positioned_bits[j][i]
    }

    if float64(sum) >= (float64(readings_count) / 2) {
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
