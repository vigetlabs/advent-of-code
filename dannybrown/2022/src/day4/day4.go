package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)


func check(e error) {
	if e != nil {
		panic(e)
	}
}

func main() {
	dat, err := os.Open("dannybrown/2022/input/day4.txt")
	check(err)
	defer dat.Close()
	scanner := bufio.NewScanner(dat)
	scanner.Split(bufio.ScanLines)
	part1_sum := 0
	part2_sum := 0
	for scanner.Scan() {
		line := scanner.Text()
		ranges := strings.Split(line, ",")
		range1 := strings.Split(ranges[0], "-")
		range2 := strings.Split(ranges[1], "-")
		range1_low, _ := strconv.Atoi(range1[0])
		range1_high, _ := strconv.Atoi(range1[1])
		range2_low, _ := strconv.Atoi(range2[0])
		range2_high, _ := strconv.Atoi(range2[1])

		if (range1_low >= range2_low && range1_high <= range2_high) || (range2_low >= range1_low && range2_high <= range1_high) {
			part1_sum++
		}
		if range2_low <= range1_high && range1_low <= range2_high {
			part2_sum++
		}
	}

	fmt.Println(part1_sum) // Part 1
	fmt.Println(part2_sum) // Part 2
}
