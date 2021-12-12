package main

import (
  "fmt"
  "os"
  "strings"
  "regexp"
)

type CaveMap map[string][]string

const debug = true
const filename = "example_md.txt"

func main() {
  data, _ := os.ReadFile(filename)

  trimmedData := strings.Trim(string(data), "\n ")
  lines := strings.Split(trimmedData, "\n")

  caveMap := loadCaveMap(lines)
  solvePartOne(caveMap)
}

func solvePartOne(caveMap CaveMap) {
  var paths []string
  current := []string{"start"}

  commenceWalking(caveMap, &paths, current)

  fmt.Println("path count:", len(paths))
}

func commenceWalking(caveMap CaveMap, paths *[]string, current []string) {
  options := caveMap[last(current)]

  for _, next := range options {
    if smallCave(next) && contains(current, next) {
      continue
    }

    newPath := append(current, next)

    if next == "end" {
      *paths = append(*paths, joinPath(newPath))
    } else {
      commenceWalking(caveMap, paths, newPath)
    }
  }
}

func smallCave(caveName string) bool {
  match, _ := regexp.MatchString(`^[a-z]`, caveName)
  return match
}

func loadCaveMap(lines []string) CaveMap {
  caveMap := make(CaveMap)

  for _, line := range lines {
    caveNames := strings.Split(line, "-")
    c1 := caveNames[0]
    c2 := caveNames[1]

    joinCaves(&caveMap, c1, c2)
    joinCaves(&caveMap, c2, c1)
  }

  return caveMap
}

func joinCaves(caveMapPtr *CaveMap, c1 string, c2 string) {
  caveMap := *caveMapPtr

  neighbors, exists := caveMap[c1]
  if exists {
    caveMap[c1] = append(neighbors, c2)
  } else {
    caveMap[c1] = []string{c2}
  }
}

func last(slice []string) string {
  return slice[len(slice) - 1]
}

func joinPath(str []string) string {
  return strings.Trim(strings.Join(str, ","), "[]")
}

func contains(slice []string, value string) bool {
  for _, v := range slice {
    if v == value {
      return true
    }
  }

  return false
}
