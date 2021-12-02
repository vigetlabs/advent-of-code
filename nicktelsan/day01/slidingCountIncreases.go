package main

import (
	"fmt"
	"io/ioutil"
	"log"
	"strconv"
	"strings"
)

func main() {

    content, err := ioutil.ReadFile("input")

     if err != nil {
          log.Fatal(err)
     }

    inputArray := strings.Split(string(content), "\n")

		var maxLength = len(inputArray) - 2
		var increases = 0

		for i := 1; i < maxLength; i++ {
			w, _ := strconv.Atoi(inputArray[i - 1])
			x, _ := strconv.Atoi(inputArray[i])
			y, _ := strconv.Atoi(inputArray[i + 1])
			z, _ := strconv.Atoi(inputArray[i + 2])

			current := x + y + z
			prev := w + x + y

			if( current > prev) {
				increases++
			}
		}
		fmt.Println(increases)
}