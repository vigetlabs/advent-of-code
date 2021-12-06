package main

import (
  "fmt"
  "os"
  "strings"
  "strconv"
)

// const debug = true
// const filename = "example.txt"
// const days = 80

const debug = false
const filename = "input.txt"
const days = 80

func main() {
  data, _ := os.ReadFile(filename)
  trimmed_data := strings.Trim(string(data), "\n ")
  split_data := strings.Split(trimmed_data, ",")

  current_fish := str_to_int(split_data)

  solve_part_one(current_fish)
}

func solve_part_one(current_fish []int) {
  for day := 1; day <= days; day++ {
    for i, fish := range current_fish {
      if fish == 0 {
        current_fish[i] = 6
        current_fish = append(current_fish, 8)
      } else {
        current_fish[i] = fish - 1
      }
    }

    if debug {
      fmt.Println("Finishing day ", day)
      fmt.Println("current_fish: ", join(current_fish))
    }
  }

  fmt.Println("Fish count: ", len(current_fish))
}

func str_to_int(strings []string) []int {
  ints := make([]int, len(strings))

  for i, str := range strings {
    ints[i], _ = strconv.Atoi(str)
  }

  return ints
}

func join(ints []int) string {
  return strings.Trim(strings.Join(strings.Fields(fmt.Sprint(ints)), ","), "[]")
}
