package main

import (
  "fmt"
  "os"
  "strings"
  "strconv"
)

const debug = false
const filename = "example.txt"

// const debug = false
// const filename = "input.txt"

type Unit struct {
  depth int
  parent *Unit
  unitType string // "pair" | "number"
  children [](*Unit)
  value int
}

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

  fmt.Println("number", number)
}

func sum(n1 string, n2 string) string {
  return "[" + n1 + "," + n2 + "]"
}

func reduce(number string) string {
  // unit := parse(number)
  // unit = reduceUnit(unit)
  // newNumber := unit.toString()
  // return newNumber

  return number
}

func parse(number string) Unit {
  current := &Unit {
    unitType: "pair",
    depth: 1,
  }

  // Time for some recurrrrrrrsiooononnnnnnnnnnn
  cursor := 1 // skip first & last characters
  for cursor < (len(number) - 1) {
    char := number[cursor:cursor+1]

    if char == "[" {
      if debug { fmt.Println("parsing", char) }

      child := Unit {
        unitType: "pair",
        depth: current.depth + 1,
        parent: current,
      }
      current.children = append(current.children, &child)
      current = &child // they grow up so fast

    } else if char == "," {
      if debug { fmt.Println("parsing", char) }
      // pass

    } else if isDigit(char) {
      nextChar := number[cursor+1:cursor+2]
      if isDigit(nextChar) {
        char = char + nextChar
      }
      if debug { fmt.Println("parsing", char) }

      value, _ := strconv.Atoi(char)
      child := Unit {
        unitType: "literal",
        depth: current.depth + 1,
        parent: current,
        value: value,
      }

      current.children = append(current.children, &child)
    } else if char == "]" {
      if debug { fmt.Println("parsing", char) }

      current = current.parent
    }

    cursor += len(char)
  }

  if debug {
    current.print()
  }

  return *current
}

func isDigit(char string) bool {
  for i := 0; i < 10; i++ {
    if char == strconv.Itoa(i) { return true }
  }

  return false
}

func (u *Unit) print() {
  spacing := ""
  for i := 0; i < u.depth - 1; i++ {
    spacing += "  "
  }

  fmt.Println(spacing, "- Unit Type:  ", u.unitType)
  fmt.Println(spacing, "- Depth:      ", u.depth)
  if (u.unitType == "literal") {
    fmt.Println(spacing, "- Value:      ", u.value)
  } else {
    fmt.Println(spacing, "- # Children: ", len(u.children))
  }

  fmt.Println("")

  for _, child := range u.children {
    child.print()
  }
}
