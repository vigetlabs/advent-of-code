package main

import (
  "fmt"
  "os"
  "strings"
  "strconv"

  "day_18/unit"
)

const debug = false
// const filename = "example.txt"

// const debug = false
const filename = "input.txt"

type Unit = unit.Unit

func main() {
  data, _ := os.ReadFile(filename)

  trimmedData := strings.Trim(string(data), "\n ")
  lines := strings.Split(trimmedData, "\n")

  number := lines[0]
  for i := 1; i < len(lines); i++ {
    nextNumber := lines[i]

    number = sum(number, nextNumber)
    number = reduce(number)
  }

  fmt.Println("number   ", number)
  fmt.Println("magnitude", magnitude(number))
}

func sum(n1 string, n2 string) string {
  return "[" + n1 + "," + n2 + "]"
}

func reduce(number string) string {
  unit := parse(number)
  unit.Reduce()
  newNumber := unit.ToString()
  // return newNumber

  return newNumber
}

func magnitude(number string) int {
  unit := parse(number)
  return unit.Magnitude()
}

func parse(number string) Unit {
  current := unit.NewBase()

  // Time for some recurrrrrrrsiooononnnnnnnnnnn
  cursor := 1 // skip first & last characters
  for cursor < (len(number) - 1) {
    char := number[cursor:cursor+1]

    if char == "[" {
      child := current.AddPair()
      current = child // they grow up so fast

    } else if isDigit(char) {
      nextChar := number[cursor+1:cursor+2]
      if isDigit(nextChar) { char = char + nextChar }

      value, _ := strconv.Atoi(char)
      current.AddLiteral(value)

    } else if char == "]" {
      current = current.Parent
    }

    cursor += len(char)
  }

  if debug {
    fmt.Println("Parsed Unit:")
    current.Print()
  }

  return *current
}

func isDigit(char string) bool {
  for i := 0; i < 10; i++ {
    if char == strconv.Itoa(i) { return true }
  }

  return false
}
