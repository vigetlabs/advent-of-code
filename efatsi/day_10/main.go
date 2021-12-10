package main

import (
  "fmt"
  "os"
  "strings"
  "sort"
)

var OPENERS = []string{"(", "[", "{", "<"}
var CLOSERS = map[string]string {
  "(" : ")",
  "[" : "]",
  "{" : "}",
  "<" : ">",
}
var INVALID_SCORES = map[string]int {
  ")" : 3,
  "]" : 57,
  "}" : 1197,
  ">" : 25137,
}
var CLOSING_SCORES = map[string]int {
  ")" : 1,
  "]" : 2,
  "}" : 3,
  ">" : 4,
}

// const debug = true
// const filename = "example.txt"

const debug = false
const filename = "input.txt"

func main() {
  data, _ := os.ReadFile(filename)

  trimmedData := strings.Trim(string(data), "\n ")
  lines := strings.Split(trimmedData, "\n")

  // solvePartOne(lines)
  solvePartTwo(lines)
}

func solvePartOne(lines []string) {
  invalidSum := 0
  for _, line := range lines {
    invalidSum += getInvalidScore(line)
  }
  fmt.Println("Part 1:", invalidSum)
}

func solvePartTwo(lines []string) {
  missingClosers := make([]string, 0)
  for _, line := range lines {
    missing := findMissingClosers(line)
    if missing != "" {
      missingClosers = append(missingClosers, missing)
    }
  }

  closingScores := calculateClosingScores(missingClosers)
  sort.Ints(closingScores)
  fmt.Println("Part 2:", closingScores[len(closingScores) / 2])
}

func getInvalidScore(line string) int {
  if debug { fmt.Println("Parsing", line) }

  characters := strings.Split(line, "")
  openChunks := make([]string, 0)

  for _, ch := range characters {
    if isOpener(ch) {
      openChunks = append(openChunks, ch)
    } else {
      if validClose(last(openChunks), ch) {
        openChunks = removeLast(openChunks)
      } else {
        if debug { fmt.Println("Found invalid:", ch) }
        return INVALID_SCORES[ch]
      }
    }
  }

  if debug { fmt.Println("Incomplete") }
  return 0
}

func findMissingClosers(line string) string {
  if debug { fmt.Println("Parsing", line) }

  characters := strings.Split(line, "")
  openChunks := make([]string, 0)

  for _, ch := range characters {
    if isOpener(ch) {
      openChunks = append(openChunks, ch)
    } else {
      if validClose(last(openChunks), ch) {
        openChunks = removeLast(openChunks)
      } else {
        if debug { fmt.Println("Found invalid:", ch) }
        return ""
      }
    }
  }

  if debug {
    fmt.Println("Incomplete", openChunks)
    fmt.Println("Closers   ", closersFor(openChunks))
  }
  return closersFor(openChunks)
}

func calculateClosingScores(missingClosers []string) []int {
  scores := make([]int, 0)

  for _, characters := range missingClosers {
    score := 0
    for _, ch := range strings.Split(characters, "") {
      score *= 5
      score += CLOSING_SCORES[ch]
    }

    scores = append(scores, score)
  }

  return scores
}

func closersFor(characters []string) string {
  toReturn := ""
  for i := len(characters) - 1; i >= 0; i-- {
    opener := characters[i]
    toReturn += CLOSERS[opener]
  }

  return toReturn
}

func isOpener(ch string) bool {
  return contains(OPENERS, ch)
}

func validClose(opener string, prospect string) bool {
  return CLOSERS[opener] == prospect
}

func last(slice []string) string {
  return slice[len(slice) - 1]
}

func removeLast(slice []string) []string {
  return slice[:len(slice) - 1]
}

func contains(slice []string, value string) bool {
  for _, v := range slice {
    if v == value {
      return true
    }
  }

  return false
}
