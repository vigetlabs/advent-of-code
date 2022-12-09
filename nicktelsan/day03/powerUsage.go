package main

import (
	"fmt"
	"io/ioutil"
	"log"
	"strconv"
	"strings"
)

func mostCommonBit(array []string, position int) string {
	zero := 0
	one := 0

	for i := 0; i < len(array); i++ {
		if array[i][position] == '0' {
			zero++
		} else {
			one++
		}
	}

	if zero > one {
		return "0"
	} else {
		return "1"
	}
}

func flipBits(number string) string {
	newNumber := ""

	for i := 0; i < len(number); i++ {
		if string(number[i]) == "0" {
			newNumber += "1"
		} else {
			newNumber += "0"
		}
	}
	return newNumber
}

func main() {
	content, err := ioutil.ReadFile("day03/input")

	if err != nil {
		log.Fatal(err)
	}

	inputArray := strings.Split(string(content), "\n")
	bitLength := len(inputArray[0])

	gamma := ""

	for i := 0; i < bitLength; i++ {
		gamma += mostCommonBit(inputArray, i)
	}

	epsilon := flipBits(gamma)

	decimalGamma, _ := strconv.ParseInt(gamma, 2, 64)
	decimalEpsilon, _ := strconv.ParseInt(epsilon, 2, 64)
	power := decimalGamma * decimalEpsilon

	fmt.Printf("gamma: %v\n", decimalGamma)
	fmt.Printf("epsilon: %v\n", decimalEpsilon)
	fmt.Printf("Power: %v\n", power)

}
