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

func rateGetter(bitsList []string, direction bool) int64 {
	bitStringLength := len(bitsList[0])
	var gammaBitList []string

	for i := 0; i < bitStringLength; i++ {
		currentBitMap := make(map[string]int)
		currentBitMap["0"] = 0
		currentBitMap["1"] = 0

		for _, bitString := range bitsList {
			currentBitMap[string(bitString[i])]++
		}

		if direction { // switch on whether it's gamma (true) or epsilon (false)
			if currentBitMap["0"] > currentBitMap["1"] {
				gammaBitList = append(gammaBitList, "0")
			} else {
				gammaBitList = append(gammaBitList, "1")
			}
		} else {
			if currentBitMap["0"] > currentBitMap["1"] {
				gammaBitList = append(gammaBitList, "1")
			} else {
				gammaBitList = append(gammaBitList, "0")
			}
		}
	}
	joinedGammaBitList := strings.Join(gammaBitList, "")
	binaryRep, _ := strconv.ParseInt(joinedGammaBitList, 2, 64)

	return binaryRep
}


func day3_part1(binaryInput []string) int64 {
	gamma := rateGetter(binaryInput, true)
	epsilon := rateGetter(binaryInput, false)
	return gamma*epsilon
}

func day3_part2(instructions []string) (int, int) {
	return 0, 0
}

func main() {
	dat, err := os.Open("../input/day3.txt")
	check(err)
	defer dat.Close()
	scanner := bufio.NewScanner(dat)
	scanner.Split(bufio.ScanLines)
	var txtlines []string

	for scanner.Scan() {
		txtlines = append(txtlines, scanner.Text())
	}

	fmt.Println(day3_part1(txtlines)) // Part 1
}
