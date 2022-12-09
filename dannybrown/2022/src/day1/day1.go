package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
)

type Elf struct {
	Number int
}

func check(e error) {
	if e != nil {
		panic(e)
	}
}

func remove(slice []Elf, s int) []Elf {
    return append(slice[:s], slice[s+1:]...)
}

func createElves(txtlines []string) []Elf {
	var elves []Elf
	currentElf := Elf{Number: 0}

	for _, eachline := range txtlines {
		if eachline == "" {
			elves = append(elves, currentElf)
			currentElf = Elf{Number: 0}
		} else {
			number, _ := strconv.Atoi(eachline)
			currentElf.Number += number
		}
	}
	return elves
}

func day1_part1(elves []Elf) int {
	greatest := 0
	for _, elf := range elves {
		if elf.Number > greatest {
			greatest = elf.Number
		}
	}
	return greatest
}

func day1_part2(elves []Elf) int {
	// brute force way :)
	greatest, secondGreatest, thirdGreatest := 0, 0, 0
	greatestIdx, secondGreatestIdx := 0, 0
	for idx, elf := range elves {
		if elf.Number > greatest {
			greatest = elf.Number
			greatestIdx = idx
		}
	}
	elves = remove(elves, greatestIdx)
	for idx, elf := range elves {
		if elf.Number > secondGreatest {
			secondGreatest = elf.Number
			secondGreatestIdx = idx
		}
	}
	elves = remove(elves, secondGreatestIdx)
	for _, elf := range elves {
		if elf.Number > thirdGreatest {
			thirdGreatest = elf.Number
		}
	}
	return greatest + secondGreatest + thirdGreatest
}

func main() {
	dat, err := os.Open("dannybrown/2022/input/day1.txt")
	check(err)
	defer dat.Close()
	scanner := bufio.NewScanner(dat)
	scanner.Split(bufio.ScanLines)
	var txtlines []string


	for scanner.Scan() {
		txtlines = append(txtlines, scanner.Text())
	}

	elves := createElves(txtlines)

	fmt.Println(day1_part1(elves)) // Part 1
	fmt.Println(day1_part2(elves)) // Part 2
}
