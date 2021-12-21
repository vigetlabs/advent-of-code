package main

import (
  "fmt"
  "os"
  "strings"
  "strconv"
)

type Pic [][]byte

const debug = true
const filename = "example.txt"

// const debug = false
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

func solvePartOne(pic Pic) {
  for i := 0; i < 2; i++ {
    pic = enHance(pic)

    if debug {
      fmt.Println("Step:", i+1)
      printPic(pic)
    }
  }

  fmt.Println("Part 1", countOn(pic))
  // Guesses:
  // 5224 too high :(
  // 5680 too high :(
  // 4999 too low  :(
}

func enHance(pic Pic) Pic {
  newDimension := len(pic) + 2

  var newPic Pic
  for x := 0; x < newDimension; x++ {
    newPic = append(newPic, make([]byte, newDimension))

    for y := 0; y < newDimension; y++ {
      newPic[x][y] = readPixel(pic, x-1, y-1)
    }
  }

  return newPic
}

func readPixel(pic Pic, centerX int, centerY int) byte {
  dimension := len(pic)

  indexStr := ""
  for dy := -1; dy <= 1; dy++ {
    for dx := -1; dx <= 1; dx++ {
      x := centerX + dx
      y := centerY + dy

      withinBounds := (x >= 0 && x < dimension && y >= 0 && y < dimension)

      if withinBounds && pic[x][y] == '#' {
        indexStr += "1"
      } else {
        indexStr += "0"
      }
    }
  }

  index := binToInt(indexStr)
  return key[index]
}

func countOn(pic Pic) int {
  dimension := len(pic)

  count := 0
  for x := 0; x < dimension; x++ {
    for y := 0; y < dimension; y++ {
      if pic[x][y] == '#' {
        count += 1
      }
    }
  }

  return count
}

func loadPic(lines []string) Pic {
  dimension := len(lines)

  var pic Pic
  for x := 0; x < dimension; x++ {
    pic = append(pic, make([]byte, dimension))

    for y := 0; y < dimension; y++ {
      pic[x][y] = lines[y][x]
    }
  }

  return pic
}

// -- helpers --

func printPic(pic Pic) {
  for y := 0; y < len(pic); y++ {
    for x := 0; x < len(pic); x++ {
      fmt.Print(string(pic[x][y]))
    }
    fmt.Println("")
  }
  fmt.Println("")
}

func binToInt(binary string) int {
  val, _ := strconv.ParseInt(binary, 2, 64)
  return int(val)
}
