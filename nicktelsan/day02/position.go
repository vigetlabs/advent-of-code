package main

import (
	"fmt"
	"io/ioutil"
	"log"
	"strconv"
	"strings"
)

func main() {

	content, err := ioutil.ReadFile("day02/input")

	if err != nil {
		log.Fatal(err)
	}

	inputArray := strings.Split(string(content), "\n")

	const FORWARD = "forward"
	const UP = "up"
	const DOWN = "down"

	var horizontal = 0
	var depth = 0

	for i := 0; i < len(inputArray); i++ {
		splitValue := strings.Split(string(inputArray[i]), " ")
		value, _ := strconv.Atoi(splitValue[1])

		switch splitValue[0] {
		case FORWARD:
			horizontal += value
		case UP:
			depth -= value
		case DOWN:
			depth += value
		}
	}

	distance := horizontal * depth
	fmt.Printf("Forward: %v\n", horizontal)
	fmt.Printf("Depth: %v\n", depth)
	fmt.Printf("Distance: %v\n", distance)
}
