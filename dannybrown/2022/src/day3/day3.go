package main

import (
	"bufio"
	"fmt"
	"os"
)

type Rucksack struct {
	compartmentOne []rune
	compartmentTwo []rune
}

func check(e error) {
	if e != nil {
		panic(e)
	}
}

func stringInSlice(a rune, list []rune) bool {
    for _, b := range list {
        if b == a {
            return true
        }
    }
    return false
}

func getPriority(r rune) int {
	if r > 96 && r < 123 {
		return int(r)-96
	} else {
		return int(r)-38
	}
}

func findSharedType(rucksack Rucksack) (rune, int) {
	for _, item := range rucksack.compartmentOne {
		if stringInSlice(item, rucksack.compartmentTwo) {
			return item, getPriority(item)
		}
	}
	return 0, 0
}

func findCommonPart2(rucksack1 Rucksack, rucksack2 Rucksack, rucksack3 Rucksack) rune {
	for _, item := range rucksack1.compartmentOne {
		if (stringInSlice(item, rucksack2.compartmentTwo) || stringInSlice(item, rucksack2.compartmentOne)) && (stringInSlice(item, rucksack3.compartmentTwo) || stringInSlice(item, rucksack3.compartmentOne)) {
			return item
		}
	}
	for _, item := range rucksack1.compartmentTwo {
		if (stringInSlice(item, rucksack2.compartmentTwo) || stringInSlice(item, rucksack2.compartmentOne)) && (stringInSlice(item, rucksack3.compartmentTwo) || stringInSlice(item, rucksack3.compartmentOne)) {
			return item
		}
	}
	return 0
}

func main() {
	dat, err := os.Open("dannybrown/2022/input/day3.txt")
	check(err)
	defer dat.Close()
	scanner := bufio.NewScanner(dat)
	scanner.Split(bufio.ScanLines)
	var rucksacks []Rucksack

	for scanner.Scan() {
		var rucksack Rucksack
		line := scanner.Text()
		length := len(line)
		firstHalf := line[:length/2]
		secondHalf := line[length/2:]

		rucksack.compartmentOne = []rune(firstHalf)
		rucksack.compartmentTwo = []rune(secondHalf)

		rucksacks = append(rucksacks, rucksack)
	}
	part1_sum := 0
	part2_sum := 0
	for _, rucksack := range rucksacks {
		var sharedTypePriority int
		_, sharedTypePriority = findSharedType(rucksack) // didn't end up needing the actual letter
		part1_sum += sharedTypePriority
	}

	for i := 0; i < len(rucksacks) - 2; i+=3 {
		rucksack1 := rucksacks[i]
		rucksack2 := rucksacks[i+1]
		rucksack3 := rucksacks[i+2]
		common := findCommonPart2(rucksack1, rucksack2, rucksack3)
		part2_sum += getPriority(common)
	}

	fmt.Println(part1_sum) // Part 1
	fmt.Println(part2_sum) // Part 2
}
