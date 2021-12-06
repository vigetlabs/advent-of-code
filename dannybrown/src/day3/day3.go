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

func getBitFrequencies(bitsList []string) []map[string]int {
	// Returns a list of maps, each map representing the counts of 0s and 1s for that index in the list
	// i.e. [{"0": 0, "1": 3}, {"0": 2, "1": 1}, {"0": 0, "1": 3}] for a list of ["101", "111", "101"]

	bitStringLength := len(bitsList[0])
	var bitFrequenciesList []map[string]int

	for i := 0; i < bitStringLength; i++ {
		currentBitMap := make(map[string]int)
		currentBitMap["0"] = 0
		currentBitMap["1"] = 0

		for _, bitString := range bitsList {
			currentBitMap[string(bitString[i])]++
		}
		bitFrequenciesList = append(bitFrequenciesList, currentBitMap)
	}
	return bitFrequenciesList
}


func determineCommonBit(bitFrequency map[string]int, isMost bool) string {
	if isMost {
		if bitFrequency["1"] >= bitFrequency["0"]  {
			return "1"
		} else {
			return "0"
		}
	} else {
		if bitFrequency["1"] >= bitFrequency["0"]  {
			return "0"
		} else {
			return "1"
		}
	}
}



func lifeSupportRating(bitsList []string, direction bool) int64 {
	bitFrequenciesList := getBitFrequencies(bitsList)

	bitsListCopy := make([]string, len(bitsList))
	copy(bitsListCopy, bitsList)

	var newBitsList []string

	for i := range bitFrequenciesList {
		if len(bitsListCopy) == 1 {
			break
		}
		newBitFrequenciesList := getBitFrequencies(bitsListCopy)
		currentBitMap := newBitFrequenciesList[i]

		commonBit := determineCommonBit(currentBitMap, direction)
		for _, bitString := range bitsListCopy {
			if string(bitString[i]) == commonBit {
				newBitsList = append(newBitsList, bitString)
			}
		}
		bitsListCopy = newBitsList
		newBitsList = []string{}
	}
	binaryRep, _ := strconv.ParseInt(bitsListCopy[0], 2, 64) // Thank you golang, very cool! :)
	return binaryRep
}


func rateGetter(bitsList []string, direction bool) int64 {
	var gammaBitList []string
	bitFrequenciesList := getBitFrequencies(bitsList)

	for _, currentBitMap := range bitFrequenciesList {
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
	binaryRep, _ := strconv.ParseInt(joinedGammaBitList, 2, 64) // Thank you golang, very cool! :)

	return binaryRep
}


func day3_part1(binaryInput []string) int64 {
	gamma := rateGetter(binaryInput, true)
	epsilon := rateGetter(binaryInput, false)
	return gamma*epsilon
}

func day3_part2(instructions []string) int64 {
	oxygen := lifeSupportRating(instructions, true)
	co2 := lifeSupportRating(instructions, false)
	return oxygen*co2
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
	fmt.Println(day3_part2(txtlines)) // Part 2
}
