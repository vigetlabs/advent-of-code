package main

import (
  "fmt"
  "os"
  "strings"
  "strconv"
)

func main() {
  data, err := os.ReadFile("input.txt")
  if err != nil {
    panic(err)
  }
  trimmed_data := strings.Trim(string(data), "\n ")
  readings := strings.Split(trimmed_data, "\n")

  // Assemble data into arrays of positioned bits
  const readings_count = 1000
  const reading_length = 12
  var array_of_arrays [reading_length][readings_count]int

  for i := 0; i < readings_count; i++ {
    for j := 0; j < reading_length; j++ {
      array_of_arrays[j][i] = parseReading(readings[i][j:j+1])
    }
  }

  // Assemble strings of most/least common bits
  var most_common_bits string
  var least_common_bits string
  for j := 0; j < reading_length; j++ {
    sum := 0
    for i := 0; i < readings_count; i++ {
      sum += array_of_arrays[j][i]
    }

    if sum > (readings_count / 2) {
      most_common_bits += "1"
      least_common_bits += "0"
    } else {
      most_common_bits += "0"
      least_common_bits += "1"
    }
  }

  gamma, err := strconv.ParseInt(most_common_bits, 2, 64)
  epsilon, err := strconv.ParseInt(least_common_bits, 2, 64)
  check(err)

  fmt.Println(gamma * epsilon)
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
