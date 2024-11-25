package board

import (
  "fmt"
)

type Position struct {
  value int
  matched bool
}

type Board struct {
  Positions [5][5]Position
}

func (b *Board) SetPosition(x int, y int, value int) {
  b.Positions[x][y].value = value
}

func (b *Board) Print() {
  for x := 0; x < 5; x++ {
    for y := 0; y < 5; y++ {
      fmt.Printf("%2d ", b.Positions[x][y].value)
    }
    fmt.Println()
  }
}

func (b *Board) UnmatchedCount() int {
  sum := 0

  for x := 0; x < 5; x++ {
    for y := 0; y < 5; y++ {
      p := b.Positions[x][y]
      if !p.matched {
        sum += p.value
      }
    }
  }

  return sum
}

func (b *Board) CheckValue(value int) {
  for x := 0; x < 5; x++ {
    for y := 0; y < 5; y++ {
      if b.Positions[x][y].value == value {
        b.Positions[x][y].matched = true
      }
    }
  }
}

func (b *Board) HasBingo() bool {
  // check rows
  for x := 0; x < 5; x++ {
    if b.bingoRow(x) {
      fmt.Println("Row: ", x)
      return true
    }
  }

  // check columns
  for y := 0; y < 5; y++ {
    if b.bingoCol(y) {
      fmt.Println("Col: ", y)
      return true
    }
  }

  return false
}

func (b *Board) bingoRow(x int) bool {
  for y := 0; y < 5; y++ {
    if b.Positions[x][y].matched == false {
      return false
    }
  }

  return true
}

func (b *Board) bingoCol(y int) bool {
  for x := 0; x < 5; x++ {
    if b.Positions[x][y].matched == false {
      return false
    }
  }

  return true
}
