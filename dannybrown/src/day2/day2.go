package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

func getHorizontalDirections() []string {
	return []string{"FORWARD"}
}
func getVerticalDirections() []string {
	return []string{"UP", "DOWN"}
}

func check(e error) {
	if e != nil {
		panic(e)
	}
}

func contains(s []string, e string) bool {
	for _, a := range s {
			if a == e {
					return true
			}
	}
	return false
}

func day2_part1(instructions []string) (int, int) {
	var horizontalPos, verticalPos int

	for _, instruction := range instructions {
		instructionSlice := strings.Split(instruction, " ")

		direction := instructionSlice[0]
		distance, _ := strconv.Atoi(instructionSlice[1])

		if contains(getHorizontalDirections(), strings.ToUpper(direction)) {
			horizontalPos += distance
		} else if contains(getVerticalDirections(), strings.ToUpper(direction)) {
			if direction == "up" {
				verticalPos -= distance
			} else {
				verticalPos += distance
			}
		}
	}
	return horizontalPos, verticalPos
}

func day2_part2(instructions []string) (int, int) {
	var horizontalPos, verticalPos, aim int

	for _, instruction := range instructions {
		instructionSlice := strings.Split(instruction, " ")

		direction := instructionSlice[0]
		distance, _ := strconv.Atoi(instructionSlice[1])

		if contains(getHorizontalDirections(), strings.ToUpper(direction)) {
			horizontalPos += distance
			verticalPos += aim * distance
		} else if contains(getVerticalDirections(), strings.ToUpper(direction)) {
			if direction == "up" {
				aim -= distance
			} else {
				aim += distance
			}
		}
	}
	return horizontalPos, verticalPos
}

func main() {
	dat, err := os.Open("../input/day2.txt")
	check(err)
	defer dat.Close()
	scanner := bufio.NewScanner(dat)
	scanner.Split(bufio.ScanLines)
	var txtlines []string

	for scanner.Scan() {
		txtlines = append(txtlines, scanner.Text())
	}

	horiz1, vert1 := day2_part1(txtlines)
	horiz2, vert2 := day2_part2(txtlines)
	fmt.Println(horiz1*vert1) // Part 1
	fmt.Println(horiz2*vert2) // Part 2
}
