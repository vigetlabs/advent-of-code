package main

import (
  "fmt"
  "os"
  "strings"
  "math"
)


// const debug = true
// const filename = "example.txt"

const debug = false
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

  solvePartOne(polymer, rules)
  solvePartTwo(polymer, rules)
}

func solvePartOne(polymer string, rules map[string]string) {
  for step := 0; step < 10; step++ {
    polymer = iteratePolymer(polymer, rules)
  }

  countPartOne(polymer)
}

func solvePartTwo(polymer string, rules map[string]string) {
  pairCounts := assemblePairCount(polymer)

  for step := 0; step < 40; step++ {
    pairCounts = iteratePairs(pairCounts, rules)
  }

  countPartTwo(pairCounts, polymer)
}

func countPartOne(polymer string) {
  counts := make(map[string]int)

  for _, char := range strings.Split(polymer, "") {
    addTo(&counts, char, 1)
  }

  if (debug) {
    fmt.Println("counts", counts)
  }

  maxCount := 0
  minCount := math.MaxInt64

  for _, count := range counts {
    if (count > maxCount) { maxCount = count }
    if (count < minCount) { minCount = count }
  }

  fmt.Println("Part 1:", maxCount - minCount)
}

func iteratePolymer(polymer string, rules map[string]string) string {
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

func iteratePairs(pairCounts map[string]int, rules map[string]string) map[string]int {
  newPairCount := make(map[string]int)

  for pair, count := range pairCounts {
    insert, exists := rules[pair]
    if exists {
      new1 := pair[:1] + insert
      new2 := insert + pair[1:]
      addTo(&newPairCount, new1, count)
      addTo(&newPairCount, new2, count)
    } else {
      fmt.Println("does this ever happen?", pair)
    }
  }

  return newPairCount
}

func assemblePairCount(polymer string) map[string]int {
  pairCounts := make(map[string]int)

  for i := 0; i < len(polymer) - 1; i++ {
    pair := polymer[i:i+2]
    addTo(&pairCounts, pair, 1)
  }

  return pairCounts
}

func countPartTwo(pairCounts map[string]int, polymer string) {
  characterCounts := make(map[string]int)

  for pair, count := range pairCounts {
    for _, char := range strings.Split(pair, "") {
      addTo(&characterCounts, char, count)
    }
  }


  first := polymer[:1]
  last := polymer[len(polymer)-1:]

  maxCount := 0
  minCount := math.MaxInt64

  for char, count := range characterCounts {
    // Characters in the middle were counted twice since the pairs overlap
    // Characters on the end need a bit of math to offset the fact that _one_ of
    //   them was just counted once, while all the others were counted twice
    var actualCount int
    if (char == first || char == last) {
      actualCount = ((count - 1) / 2) + 1
    } else {
      actualCount = count / 2
    }

    if (actualCount > maxCount) { maxCount = actualCount }
    if (actualCount < minCount) { minCount = actualCount }
  }

  if (debug) {
    fmt.Println("characterCounts:", characterCounts)
    fmt.Println("maxCount:", maxCount)
    fmt.Println("minCount:", minCount)
  }

  fmt.Println("Part 2:", maxCount - minCount)
}

func addTo(countsPtr *map[string]int, key string, count int) {
  counts := *countsPtr

  _, exists := counts[key]
  if exists {
    counts[key] += count
  } else {
    counts[key] = count
  }
}
