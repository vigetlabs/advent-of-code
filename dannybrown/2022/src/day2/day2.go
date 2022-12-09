package main

import (
	"bufio"
	"fmt"
	"os"
)

type Round struct {
	theirs rune
	mine rune
	mine2 rune
	outcome1 string
	total1 int
	outcome2 string
	total2 int

}

func check(e error) {
	if e != nil {
		panic(e)
	}
}

// func remove(slice []Elf, s int) []Elf {
//     return append(slice[:s], slice[s+1:]...)
// }


func checkOutcomePart1(theirs rune, mine rune) string {
	if (theirs == 'A' && mine == 'X') || (theirs == 'B' && mine == 'Y') || (theirs == 'C' && mine == 'Z') {
		return "draw"
	} else if (theirs == 'A' && mine == 'Y') || (theirs == 'B' && mine == 'Z') || (theirs == 'C' && mine == 'X') {
		return "win"
	} else {
		return "lose"
	}
}

func totalOutcomePart1(round Round) int {
	sum := 0
	if round.outcome1 == "win" {
		sum += 6
	} else if round.outcome1 == "draw" {
		sum += 3
	}

	if round.mine == 'X' {
		sum += 1
	} else if round.mine == 'Y' {
		sum += 2
	} else {
		sum += 3
	}
	return sum
}

func totalOutcomePart2(round Round) int {
	sum := 0
	if round.outcome2 == "win" {
		sum += 6
	} else if round.outcome2 == "draw" {
		sum += 3
	}

	if round.mine2 == 'A' {
		sum += 1
	} else if round.mine2 == 'B' {
		sum += 2
	} else {
		sum += 3
	}
	return sum
}


func checkOutcomePart2(theirs rune, mine rune) (rune, string) {
	if mine == 'X' {
		// lose
		if theirs == 'A' {
			return 'C', "lose"
		} else if theirs == 'B' {
			return 'A', "lose"
		} else {
			return 'B', "lose"
		}
	} else if mine == 'Y' {
		return theirs, "draw" // draw
	} else {
		//win
		if theirs == 'A' {
			return 'B', "win"
		} else if theirs == 'B' {
			return 'C', "win"
		} else {
			return 'A', "win"
		}
	}
}
func main() {
	dat, err := os.Open("dannybrown/2022/input/day2.txt")
	check(err)
	defer dat.Close()
	scanner := bufio.NewScanner(dat)
	scanner.Split(bufio.ScanLines)
	var rounds []Round

	for scanner.Scan() {
		var round Round
		line := scanner.Text()
		rune_line := []rune(line)
		// Could there be a situation where the space is not at index 1?
		round.theirs = rune_line[0]
		round.mine = rune_line[2]
		round.outcome1 = checkOutcomePart1(round.theirs, round.mine)
		round.mine2, round.outcome2 = checkOutcomePart2(round.theirs, round.mine)
		round.total1 = totalOutcomePart1(round)
		round.total2 = totalOutcomePart2(round)
		rounds = append(rounds, round)
	}
	part1_sum := 0
	part2_sum := 0
	for _, round := range rounds {
		part1_sum += round.total1
		part2_sum += round.total2
	}
	fmt.Println(part1_sum) // Part 1
	fmt.Println(part2_sum) // Part 2
}
