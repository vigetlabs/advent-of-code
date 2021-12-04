package board

// import (
//   "fmt"
// )

type Position struct {
  Value int
  matched bool
}

type Board struct {
  Positions [5][5]Position
}

func (b *Board) SetPosition(x int, y int, value int) {
  b.Positions[x][y].Value = value
}
