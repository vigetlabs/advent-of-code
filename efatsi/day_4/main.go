package main

import (
  "fmt"
  "os"
  "strings"
  "strconv"
  // "regexp"

  "day_4/board"
)

func main() {
  // data, _ := os.ReadFile("example_mine.txt")
  // data, _ := os.ReadFile("example.txt")
  data, _ := os.ReadFile("input.txt")

  trimmed_data := strings.Trim(string(data), "\n ")
  board_inputs := strings.Split(trimmed_data, "\n")

  boards := make([]board.Board, 0)
  commands := strings.Split(board_inputs[0], ",")

  // Load in board data
  for i := 1; i < len(board_inputs); i++ {
    input_line := board_inputs[i]

    if (input_line == "") {
      new_board := board.Board{}

      for j := 0; j < 5; j++ {
        inputs := board_inputs[i + j + 1]
        input_ints := strings.Fields(inputs)

        for k := 0; k < 5; k++ {
          // fmt.Println(input_ints[k])
          value, _ := strconv.Atoi(input_ints[k])
          new_board.SetPosition(j, k, value)
        }
      }

      boards = append(boards, new_board)
    }
  }

  solvePartOne(boards, commands)
}

func solvePartOne(boards []board.Board, commands []string)  {
  for _, command := range commands {
    value, _ := strconv.Atoi(command)
    fmt.Println("checking ", value)

    for i := 0; i < len(boards); i++ {
      board := &boards[i]
      board.CheckValue(value)
      if board.HasBingo() {
        fmt.Println("got a winner: ")
        board.Print()

        fmt.Println("Unmatched count:", board.UnmatchedCount())
        fmt.Println("Value: ", board.UnmatchedCount() * value)
        panic("stop")
      }
    }
  }
}
