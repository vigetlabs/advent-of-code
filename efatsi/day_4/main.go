package main

import (
  "fmt"
  "os"
  "strings"
  // "regexp"
  "strconv"

  "day_4/board"
)

func main() {
  data, _ := os.ReadFile("example.txt")
  // data, _ := os.ReadFile("input.txt")

  trimmed_data := strings.Trim(string(data), "\n ")
  board_inputs := strings.Split(trimmed_data, "\n")

  boards := make([]board.Board, 0)
  // commands := board_inputs[0]

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

  fmt.Println(boards)

  // boards created, now process commands...
}
