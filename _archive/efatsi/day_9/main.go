package main

import (
  "fmt"
  "os"
  "strings"
  "strconv"
  "sort"
)

// const width = 10
// const height = 5
// const filename = "example.txt"

const width = 100
const height = 100
const filename = "input.txt"

type Heightmap [height][width]Position

type Position struct {
  x int
  y int
  value int
}

func main() {
  data, _ := os.ReadFile(filename)

  trimmedData := strings.Trim(string(data), "\n ")
  ventLines := strings.Split(trimmedData, "\n")

  heightmap := loadHeightmap(ventLines)

  lowPoints := make([]Position, 0)
  for y := 0; y < height; y++ {
    for x := 0; x < width; x++ {
      position := heightmap[y][x]

      if isLowpoint(heightmap, position) {
        lowPoints = append(lowPoints, position)
      }
    }
  }

  solvePartOne(lowPoints)
  solvePartTwo(heightmap, lowPoints)
}

func solvePartOne(lowPoints []Position) {
  riskLevel := 0
  for _, point := range lowPoints {
    riskLevel += point.value + 1
  }
  fmt.Println("Part 1:", riskLevel)
}

func solvePartTwo(heightmap Heightmap, lowPoints []Position) {
  basinSizes := make([]int, 0)

  for _, position := range lowPoints {
    basinSizes = append(basinSizes, calculateBasinSize(heightmap, position))
  }

  sort.Ints(basinSizes)
  lastThree := basinSizes[len(basinSizes) - 3:]
  product := 1
  for _, size := range lastThree {
    product = product * size
  }
  fmt.Println("Part 2:", product)
}

func calculateBasinSize(heightmap Heightmap, position Position) int {
  basin := make([]Position, 0)
  nextPositions := []Position{position}

  for len(nextPositions) > 0 {
    position, nextPositions = nextPositions[0], nextPositions[1:]

    basin = append(basin, position)

    for _, neighbor := range position.neighbors(heightmap) {
      if neighbor.value < 9 && !contains(basin, neighbor) && !contains(nextPositions, neighbor) {
        nextPositions = append(nextPositions, neighbor)
      }
    }
  }

  return len(basin)
}

func loadHeightmap(ventLines []string) Heightmap {
  var heightmap Heightmap

  for y, line := range ventLines {
    ventReadings := strings.Split(line, "")

    for x, reading := range ventReadings {
      val, _ := strconv.Atoi(reading)
      heightmap[y][x] = Position{x, y, val}
    }
  }

  return heightmap
}

func isLowpoint(heightmap Heightmap, position Position) bool {
  for _, neighbor := range position.neighbors(heightmap) {
    if neighbor.value <= position.value {
      // Found a neighbor who's equal or lower, return false
      return false
    }
  }

  // Else, all adjascent values are higher, we have a low point!
  return true
}

func (position *Position) neighbors(heightmap Heightmap) []Position {
  neighbors := make([]Position, 0)

  x := position.x
  y := position.y

  // Check Up
  if y > 0 {
    neighbors = append(neighbors, heightmap[y-1][x])
  }

  // Check Down
  if y < height - 1 {
    neighbors = append(neighbors, heightmap[y+1][x])
  }

  // Check Left
  if x > 0 {
    neighbors = append(neighbors, heightmap[y][x-1])
  }

  // Check Right
  if x < width - 1 {
    neighbors = append(neighbors, heightmap[y][x+1])
  }

  return neighbors
}

func contains(slice []Position, value Position) bool {
  for _, v := range slice {
    if v == value {
      return true
    }
  }

  return false
}
