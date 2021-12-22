package part2

import (
  "fmt"
)

type Universes = map[GameState]int

type GameState struct {
  p1Position int
  p1Score int
  p2Position int
  p2Score int
}

// Rolling a Dirac die 3 times:
var diracProbabilities = map[int]int {
  3: 1,
  4: 3,
  5: 6,
  6: 7,
  7: 6,
  8: 3,
  9: 1,
}

var p1WinCounts = 0
var p2WinCounts = 0

func Solve(p1Position int, p2Position int) {
  openUniverses := make(Universes)
  closedUniverses := make(Universes)
  initialState := GameState {
    p1Position: p1Position,
    p1Score: 0,
    p2Position: p2Position,
    p2Score: 0,
  }

  openUniverses[initialState] += 1

  for i := 0; len(openUniverses) > 0; i++ {
    fmt.Println("Stepping", i)
    openUniverses = step(openUniverses, &closedUniverses)
  }

  fmt.Println("p1WinCounts", p1WinCounts)
  fmt.Println("p2WinCounts", p2WinCounts)
}

func step(openUniverses Universes, closedUniverses *Universes) Universes {
  // Roll dirac dice for first player
  nextUniverses := make(Universes)
  for gameState, count := range openUniverses {
    for roll, occurance := range diracProbabilities {
      newPosition := mod1(gameState.p1Position + roll, 10)
      newScore := gameState.p1Score + newPosition

      newGameState := GameState {
        p1Position: newPosition,
        p1Score: newScore,
        p2Position: gameState.p2Position,
        p2Score: gameState.p2Score,
      }

      if newScore < 21 {
        nextUniverses[newGameState] += count * occurance
      } else {
        (*closedUniverses)[newGameState] += count * occurance
        p1WinCounts += count * occurance
      }
    }
  }

  // Roll dirac dice for second player
  nextNextUniverses := make(Universes)
  for gameState, count := range nextUniverses {
    for roll, occurance := range diracProbabilities {
      newPosition := mod1(gameState.p2Position + roll, 10)
      newScore := gameState.p2Score + newPosition

      newGameState := GameState {
        p1Position: gameState.p1Position,
        p1Score: gameState.p1Score,
        p2Position: newPosition,
        p2Score: newScore,
      }

      if newScore < 21 {
        nextNextUniverses[newGameState] += count * occurance
      } else {
        (*closedUniverses)[newGameState] += count * occurance
        p2WinCounts += count * occurance
      }
    }
  }

  return nextNextUniverses
}

func mod1(input int, last int) int {
  val := input % last

  if val == 0 {
    return last
  } else {
    return val
  }
}
