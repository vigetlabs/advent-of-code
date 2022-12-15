package main

import (
	"bufio"
	"fmt"
	"os"
)

func check(e error) {
	if e != nil {
		panic(e)
	}
}

func isUnique(input string) bool {
	seen := make(map[rune]bool)
	for _, char := range input {
		if seen[char] {
			return false
		}
		seen[char] = true
	}
	return true
}

func checkSubstr(sub string, input string, index int, length int) int {
	if len(sub) == length && isUnique(sub) {
		return index
	}

	var updatedSub string
	if len(sub) == length {
		updatedSub = sub[1:] + string(input[index+1])
	} else {
		updatedSub = sub + string(input[index+1])
	}
	return checkSubstr(updatedSub, input, index+1, length)

}

func main() {
	dat, err := os.Open("dannybrown/2022/input/day6.txt")
	check(err)
	defer dat.Close()
	scanner := bufio.NewScanner(dat)
	scanner.Split(bufio.ScanLines)
	scanner.Scan()
	input := scanner.Text()

	fmt.Println(checkSubstr(string(input[0]), input, 0, 4)+1) // Part 1
	fmt.Println(checkSubstr(string(input[0]), input, 0, 14)+1) // Part 2
}
