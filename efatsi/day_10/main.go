package main

import (
  "fmt"
  "os"
  "strings"
)

var openers = []string{"(", "[", "{", "<"}
var closers = []string{")", "]", "}", ">"}
var scores = map[string]int {
  ")" : 3,
  "]" : 57,
  "}" : 1197,
  ">" : 25137,
}

// const debug = true
// const filename = "example.txt"

const debug = false
const filename = "input.txt"

func main() {
  data, _ := os.ReadFile(filename)

  trimmedData := strings.Trim(string(data), "\n ")
  lines := strings.Split(trimmedData, "\n")

  solvePartOne(lines)
}

func solvePartOne(lines []string) {
  invalidSum := 0
  for _, line := range lines {
    invalidSum += getInvalidScore(line)
  }
  fmt.Println("Part 1:", invalidSum)
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
        return scores[ch]
      }
    }
  }

  if debug { fmt.Println("Incomplete") }
  return 0
}

func isOpener(ch string) bool {
  return contains(openers, ch)
}

func validClose(opener string, prospect string) bool {
  return indexOf(opener, openers) == indexOf(prospect, closers)
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

func indexOf(element string, data []string) (int) {
  for k, v := range data {
    if element == v {
      return k
    }
  }
  return -1
}
