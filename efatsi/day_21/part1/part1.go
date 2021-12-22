package part1

import (
  "fmt"
)

type Die struct {
  rollCount int
  value int
}

func Solve(p1Position int, p2Position int) {
  p1Score := 0
  p2Score := 0

  die := Die { rollCount: 0, value: 0 }

  fmt.Println("START:")
  printGame(p1Position, p1Score, p2Position, p2Score)

  for i := 0; (p1Score < 1000 && p2Score < 1000); i++ {
    fmt.Println("ROUND:", i + 1)
    fmt.Println("")

    // Roll Player 1
    p1Roll := die.rollThree()
    p1Position = mod1(p1Position + p1Roll, 10)
    p1Score += p1Position

    // Roll Player 2 (unless Player 1 won)
    if p1Score < 1000 {
      p2Roll := die.rollThree()
      p2Position = mod1(p2Position + p2Roll, 10)
      p2Score += p2Position
    }

    printGame(p1Position, p1Score, p2Position, p2Score)
  }

  fmt.Println("Part 1", p2Score * die.rollCount)
}

func (d *Die) rollThree() int {
  sum := 0
  for r := 0; r < 3; r++ {
    sum += d.roll()
  }
  return sum
}

func (d *Die) roll() int {
  d.rollCount += 1
  d.value = mod1(d.value + 1, 10)

  return d.value
}

func mod1(input int, last int) int {
  val := input % last

  if val == 0 {
    return last
  } else {
    return val
  }
}

func printGame(p1Position int, p1Score int, p2Position int, p2Score int)  {
  fmt.Println("")
  fmt.Println("Clover Position:", p1Position)
  fmt.Println("Clover Score:   ", p1Score)
  fmt.Println("")
  fmt.Println("Elias  Position:", p2Position)
  fmt.Println("Elias  Score:   ", p2Score)

  fmt.Println("- - - - - - - - -")
  fmt.Println("")
}
