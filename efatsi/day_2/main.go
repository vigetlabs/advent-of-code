package main

import (
  "fmt"
  "os"
  "strings"
  "regexp"
  "strconv"
)

func main() {
  data, err := os.ReadFile("input.txt")
  if err != nil {
    panic(err)
  }

  commands := strings.Split(string(data), "\n")
  horizontal := 0
  depth := 0

  for i := 0; i < len(commands); i++ {
    command := commands[i]
    intString := regexp.MustCompile(`\w+\s`).ReplaceAllString(command, "")
    number, err := strconv.Atoi(intString)

    if (err != nil) {
      continue
    }

    if (strings.Contains(command, "forward")) {
      horizontal += number
    } else if (strings.Contains(command, "down")) {
      depth += number
    } else if (strings.Contains(command, "up")) {
      depth -= number
    }
  }

  fmt.Println(horizontal * depth)
}
