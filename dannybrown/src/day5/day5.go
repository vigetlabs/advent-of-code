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

type line struct {
	x1, y1, x2, y2 int
}

func getOnlyHorizontalAndVerticalLines(input []line) []line {
	var lines []line
	for _, line := range input {
		if line.x1 == line.x2 {
			lines = append(lines, line)
		} else if line.y1 == line.y2 {
			lines = append(lines, line)
		}
	}
	return lines
}

func getMinMaxX(input []line) (int, int) {
	minX := 100
	var maxX int
	for _, line := range input {
		if line.x1 < minX {
			minX = line.x1
		}
		if line.x2 > maxX {
			maxX = line.x2
		}
	}
	return minX, maxX
}

func getMinMaxY(input []line) (int, int) {
	minY := 100
	var maxY int
	for _, line := range input {
		if line.y1 < minY {
			minY = line.y1
		}
		if line.y2 > maxY {
			maxY = line.y2
		}
	}
	return minY, maxY
}

func min(a, b int) int {
	if a < b {
			return a
	}
	return b
}

func max(a, b int) int {
	if a > b {
			return a
	}
	return b
}

func main() {
	dat, err := os.Open("../input/day5.txt")
	check(err)
	defer dat.Close()
	scanner := bufio.NewScanner(dat)
	scanner.Split(bufio.ScanLines)
	var input []line
	// block := []string{}
	// var boardsArray [][5][5]boardSquare
	// i := 0
	for scanner.Scan() {

			lineStr := scanner.Text()
			coords := strings.Split(lineStr, " -> ")
			coords1 := strings.Split(coords[0], ",")
			coords2 := strings.Split(coords[1], ",")
			x1, _ := strconv.Atoi(coords1[0])
			y1, _ := strconv.Atoi(coords1[1])
			x2, _ := strconv.Atoi(coords2[0])
			y2, _ := strconv.Atoi(coords2[1])
			input = append(input, line{x1, y1, x2, y2})


		}
		minX, maxX := getMinMaxX(input)
		minY, maxY := getMinMaxY(input)
		fmt.Println(minX, maxX, minY, maxY)
		part1Lines := getOnlyHorizontalAndVerticalLines(input)
		fmt.Println(part1Lines)
		floorMap := make([][]int, 1000)
		for i := range floorMap {
				floorMap[i] = make([]int, 1000)
		}
		//413,133 -> 942,662

		for _, line := range input {
			realx1 := min(line.x1, line.x2)
			realx2 := max(line.x1, line.x2)
			realy1 := min(line.y1, line.y2)
			realy2 := max(line.y1, line.y2)
			// iterator := 0
			slope := realx2 - realx1
			if realx1 == realx2 {
				for y := realy1; y <= realy2; y++ {
					floorMap[realx1][y] += 1
				}
			} else if realy1 == realy2 {
				for x := realx1; x <= realx2; x++ {
					floorMap[x][realy1] += 1
				}
			} else {
			for i := 0; i <= slope; i++ {
				// fmt.Println("new loop")
				floorMap[realx1+i][realy1+i] += 1
				// fmt.Println(iterator)
				// fmt.Println(realy1 == realy2)
				// j := realy1 + iterator
				// fmt.Println("realy1", realy1)
				// fmt.Println("realy2", realy2)
				// fmt.Println("realx1", realx1)
				// fmt.Println("realx2", realx2)

				// fmt.Println("j", j)
				// fmt.Println("i", i)
				// if i > realx2 || j > realy2 {
				// 	break
				// } else {
				// // for j := realy1; j <= realy2; j++ {
				// 	floorMap[i][j] += 1
				// // }
				// 	iterator++

				// }

			}
		}
		}
		sum := 0
		for i := range floorMap {
			for j := range floorMap[i] {
				if floorMap[i][j] > 1 {
					sum++
				}
			}
		}
		fmt.Println(sum)
	}


