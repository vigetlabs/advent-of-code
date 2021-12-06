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
const days = 256

func main() {
  data, _ := os.ReadFile(filename)
  trimmed_data := strings.Trim(string(data), "\n ")
  split_data := strings.Split(trimmed_data, ",")

  current_fish := str_to_int(split_data)

  // solve_part_one(current_fish)
  solve_part_two(current_fish)
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

func solve_part_two(current_fish []int) {
  fish_stats := initialize_fish_stats(current_fish)

  for day := 1; day <= days; day++ {
    next_fish_stats := make(map[int]int)

    for i := 0; i <= 8; i++ {
      if i == 0 {
        next_fish_stats[6] += fish_stats[0]
        next_fish_stats[8] += fish_stats[0]
      } else {
        next_fish_stats[i - 1] += fish_stats[i]
      }
    }

    fish_stats = next_fish_stats

    if debug {
      fmt.Println("Day         ", day)
      fmt.Println("Fish stats: ", fish_stats)
      fmt.Println()
    }
  }

  fmt.Println("Fish count: ", count_fish(fish_stats))
}

func initialize_fish_stats(current_fish []int) map[int]int {
  fish_stats := make(map[int]int)
  for i := 0; i <= 8; i++ {
    fish_stats[i] = 0
  }

  for _, fish := range current_fish {
    fish_stats[fish] += 1
  }

  return fish_stats
}

func count_fish(fish_stats map[int]int) int  {
  sum := 0
  for i := 0; i <= 8 ; i++ {
    sum += fish_stats[i]
  }
  return sum
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
