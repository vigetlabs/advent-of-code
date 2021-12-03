package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
)
func check(e error) {
	if e != nil {
		panic(e)
	}
}

func day1_part1(input []string) int {
	var sum int
	var inputLength int = len(input)

	for i := 0; i < inputLength; i++ {
		if i == inputLength - 1 {
			break
		}
		val1, _ := strconv.Atoi(input[i])
		val2, _ := strconv.Atoi(input[i+1])
		if val1 < val2 {
			sum++
		}
	}
	return sum
}

func day1_part2(input []string) int {
	var sum int
	var inputLength int = len(input)

	for i := 0; i < inputLength; i++ {
		if i + 2 == inputLength - 1 {
			break
		}
		val1, _ := strconv.Atoi(input[i])
		val2, _ := strconv.Atoi(input[i+1])
		val3, _ := strconv.Atoi(input[i+2])
		val4, _ := strconv.Atoi(input[i+3])

		firstBatch := val1 + val2 + val3
		secondBatch := val2 + val3 + val4
		if firstBatch < secondBatch {
			sum++
		}
	}
	return sum
}

func main() {
	dat, err := os.Open("../input/day1.txt")
	check(err)
	defer dat.Close()
	scanner := bufio.NewScanner(dat)
	scanner.Split(bufio.ScanLines)
	var txtlines []string

	for scanner.Scan() {
		txtlines = append(txtlines, scanner.Text())
	}

	fmt.Println(day1_part1(txtlines)) // Part 1
	fmt.Println(day1_part2(txtlines)) // Part 2
}
