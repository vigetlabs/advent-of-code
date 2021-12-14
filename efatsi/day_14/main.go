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

  polymer := lines[0]
  rules := make(map[string]string)
  for i := 2; i < len(lines); i++ {
    ruleStr := strings.Split(lines[i], " -> ")
    rules[ruleStr[0]] = ruleStr[1]
  }

  for step := 0; step < 10; step++ {
    polymer = iterate(polymer, rules)
  }

  solvePartOne(polymer)
}

func iterate(polymer string, rules map[string]string) string {
  polymerSlice := strings.Split(polymer, "")
  nextSlice := make([]string, 0)

  for i := 0; i < len(polymerSlice); i++ {
    nextSlice = append(nextSlice, polymerSlice[i])
    if (i + 1 == len(polymerSlice)) { continue }

    pair := polymerSlice[i] + polymerSlice[i+1]
    insert, exists := rules[pair]
    if exists {
      nextSlice = append(nextSlice, insert)
    }
  }

  return strings.Join(nextSlice, "")
}

func solvePartOne(polymer string) {
  polymerSlice := strings.Split(polymer, "")
  counts := make(map[string]int)

  for _, char := range polymerSlice {
    _, exists := counts[char]
    if exists {
      counts[char] += 1
    } else {
      counts[char] = 1
    }
  }

  // fmt.Println("counts", counts)

  maxCount := 0
  minCount := 1000000
  for _, count := range counts {
    if count > maxCount {
      maxCount = count
    } else if count < minCount {
      minCount = count
    }
  }

  fmt.Println("Part 1:", maxCount - minCount)

  // 3515 too low :(
}
