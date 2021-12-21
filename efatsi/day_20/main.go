package main

import (
  "fmt"
  "os"
  "strings"
)

type Pic [][]byte

const debug = true
const filename = "example.txt"

// const debug = false-
// const filename = "input.txt"

// Image Enhance Algorithm
var key string

func main() {
  data, _ := os.ReadFile(filename)

  trimmedData := strings.Trim(string(data), "\n ")
  lines := strings.Split(trimmedData, "\n")

  key = lines[0]
  startingPic := loadPic(lines[2:])
  if debug { printPic(startingPic) }

  solvePartOne(startingPic)
}

func solvePartOne(startingPic Pic) {
  fmt.Println("key", key)
  fmt.Println("TODO", startingPic)
}

func loadPic(lines []string) Pic {
  var pic Pic
  dimension := len(lines)

  for x := 0; x < dimension; x++ {
    pic = append(pic, make([]byte, dimension))

    for y := 0; y < dimension; y++ {
      pic[x][y] = lines[y][x]
    }
  }

  return pic
}

func printPic(pic Pic) {
  for y := 0; y < len(pic); y++ {
    for x := 0; x < len(pic); x++ {
      fmt.Print(string(pic[x][y]))
    }
    fmt.Println("")
  }
  fmt.Println("")
}
