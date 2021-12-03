package main

import (
	"fmt"
	"io/ioutil"
	"log"
	"strconv"
	"strings"
)

func divideArray(array []string, position int) ([]string, []string) {
	if len(array) == 1 {
		return array, array
	}

	zero := 0
	one := 0

	leastCommonArray := make([]string, 0)
	mostCommonArray := make([]string, 0)

	for i := 0; i < len(array); i++ {
		if array[i][position] == '0' {
			zero++
		} else {
			one++
		}
	}

	mostCommon := ""

	if zero > one {
		mostCommon = "0"
	} else {
		mostCommon = "1"
	}

	for i := 0; i < len(array); i++ {
		if string(array[i][position]) == mostCommon {
			mostCommonArray = append(mostCommonArray, array[i])
		} else {
			leastCommonArray = append(leastCommonArray, array[i])
		}
	}

	return mostCommonArray, leastCommonArray
}

func main() {
	content, err := ioutil.ReadFile("day03/input")

	if err != nil {
		log.Fatal(err)
	}

	inputArray := strings.Split(string(content), "\n")
	bitLength := len(inputArray[0])

	generator, scrubber := divideArray(inputArray, 0)

	for i := 1; i < bitLength; i++ {
		generator, _ = divideArray(generator, i)
		_, scrubber = divideArray(scrubber, i)
	}

	decimalGenerator, _ := strconv.ParseInt(generator[0], 2, 64)
	decimalScrubber, _ := strconv.ParseInt(scrubber[0], 2, 64)
	lifeSupport := decimalGenerator * decimalScrubber

	fmt.Printf("O2 Generator Rating: %v\n", decimalGenerator)
	fmt.Printf("CO2 Scrubber Rating: %v\n", decimalScrubber)
	fmt.Printf("Life Support Rating: %v\n", lifeSupport)

}
