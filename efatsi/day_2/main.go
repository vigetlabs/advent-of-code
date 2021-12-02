package main

import (
  "fmt"
  "os"
  "strings"
  "strconv"
)

func main() {
  data, err := os.ReadFile("input.txt")
  if err != nil {
    panic(err)
  }
  trimmed_data := strings.Trim(string(data), "\n ")
  commands := strings.Split(trimmed_data, "\n")

  horizontal := 0
  depth := 0
  aim := 0

  for i := 0; i < len(commands); i++ {
    command := commands[i]

    intString := strings.Split(command, " ")[1]
    number, err := strconv.Atoi(intString)

    if (err != nil) {
      continue
    }

    if (strings.Contains(command, "forward")) {
      horizontal += number
      depth += number * aim
    } else if (strings.Contains(command, "down")) {
      aim += number
    } else if (strings.Contains(command, "up")) {
      aim -= number
    }
  }

  fmt.Println(horizontal * depth)
}
