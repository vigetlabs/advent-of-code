package main

import (
	"bufio"
	"errors"
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

type fish struct {
	counter int
}

func update(f *fish) (fish, error) {
	if f.counter == 0 {
		f.counter = 6
		return fish{8}, nil
	} else {
		f.counter --
		return fish{}, errors.New("no new fish")
	}
}

func day6_part1 (lanternFishList []fish) int {
	var newFishList []fish
	for i := 0; i <= 80; i++ {
		fmt.Println("Iteration: ", i)
		lanternFishList = append(lanternFishList, newFishList...)

		newFishList = []fish{}
		for index, lanternFish := range lanternFishList {
			newFish, err := update(&lanternFish)
			if err == nil {
				newFishList = append(newFishList, newFish)
			}
			lanternFishList[index] = lanternFish
		}
	}
	return len(lanternFishList)

}

func main() {
	dat, err := os.Open("../input/day6.txt")
	check(err)
	defer dat.Close()
	scanner := bufio.NewScanner(dat)
	scanner.Split(bufio.ScanLines)
	var lanternFishList []fish
	// block := []string{}
	// var boardsArray [][5][5]boardSquare
	// i := 0
	for scanner.Scan() {
			lineStr := scanner.Text()
			counters := strings.Split(lineStr, ",")
			for _, counter := range counters {
				var lanternFish fish
				intCounter, _ := strconv.ParseInt(counter, 10, 64)
				lanternFish.counter = int(intCounter)
				lanternFishList = append(lanternFishList, lanternFish)
			}
		}
	lanternFishMap := make(map[int]int)
	for _, lanternFish := range lanternFishList {
		lanternFishMap[lanternFish.counter]++
		// lanternFishMap[index] = lanternFish.counter
	}

	// fmt.Println(day6_part1(lanternFishList))

	// fmt.Println(lanternFishList)
	fmt.Println(lanternFishMap)
	/*
		[
			{
				0: 1
			},
			{
				1: 1
			},
			{
				2: 12
			}

		]
	**/
	for i := 0; i < 256; i++ {
		fmt.Println("Iteration: ", i)
		// fmt.Println(len(lanternFishMap))
		newFish := lanternFishMap[0]
		lanternFishMap[9] = newFish

		lanternFishMap[0] = lanternFishMap[1]
		lanternFishMap[1] = lanternFishMap[2]
		lanternFishMap[2] = lanternFishMap[3]
		lanternFishMap[3] = lanternFishMap[4]
		lanternFishMap[4] = lanternFishMap[5]
		lanternFishMap[5] = lanternFishMap[6]
		lanternFishMap[6] = lanternFishMap[7] + newFish
		lanternFishMap[7] = lanternFishMap[8]
		lanternFishMap[8] = lanternFishMap[9]
		lanternFishMap[9] = 0
	}
	var sum int64
	for _, value := range lanternFishMap {
		sum += int64(value)
	}
	fmt.Println(sum)
		// testLanternFishList := []fish{fish{3}, fish{4}, fish{3}, fish{1}, fish{2}}
	}


