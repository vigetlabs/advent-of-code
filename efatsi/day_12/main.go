package main

import (
  "fmt"
  "os"
  "strings"
  // "regexp"
  // "sort"
)

type CaveMap map[string][]string

const debug = true
const filename = "example_sm.txt"

// start-A
// start-b
// A-c
// A-b
// b-d
// A-end
// b-end

func main() {
  data, _ := os.ReadFile(filename)

  trimmedData := strings.Trim(string(data), "\n ")
  lines := strings.Split(trimmedData, "\n")

  caveMap := loadCaveMap(lines)
  fmt.Println("caveMap:", caveMap)
}

func loadCaveMap(lines []string) CaveMap {
  caveMap := make(CaveMap)

  for _, line := range lines {
    if debug { fmt.Println("Parsing line:", line) }

    caveNames := strings.Split(line, "-")
    c1 := caveNames[0]
    c2 := caveNames[1]

    neighbors, exists := caveMap[c1]
    if exists {
      caveMap[c1] = append(neighbors, c2)
    } else {
      caveMap[c1] = []string{c2}
    }
  }

  return caveMap
}
