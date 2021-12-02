package main

import (
	"fmt"
	"io/ioutil"
	"log"
	"strconv"
	"strings"
)

func main() {

	content, err := ioutil.ReadFile("day01/input")

	if err != nil {
		log.Fatal(err)
	}

	inputArray := strings.Split(string(content), "\n")

	var increases = 0

	for i := 1; i < len(inputArray); i++ {
		current, _ := strconv.Atoi(inputArray[i])
		prev, _ := strconv.Atoi(inputArray[i-1])
		if current > prev {
			increases++
		}
	}
	fmt.Println(increases)
}
