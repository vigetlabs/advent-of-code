package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

const SIZE_TO_CHECK = 100000

func check(e error) {
	if e != nil {
		panic(e)
	}
}

type Node struct {
    text     string
	size     int
    children []*Node
	parent   *Node
}

func main() {
	dat, err := os.Open("dannybrown/2022/input/day7.txt")
	check(err)
	defer dat.Close()
	scanner := bufio.NewScanner(dat)
	scanner.Split(bufio.ScanLines)
	scanner.Scan() // Skip first line cd /
	// input := scanner.Text()
	root := Node{
		text:     "/",
		size:     0,
		children: []*Node{},
		parent:  nil,
	}
	curNode := &root
	for scanner.Scan() {
		input := scanner.Text()
		fmt.Println(input)
		if input == "$ ls" {
			//list current dir
			for {
				scanner.Scan()
				nextInput := scanner.Text()
				fmt.Println(nextInput)

				if nextInput[0] == '$' {
					// No longer listing files/dirs
					break
				}
				splitInput := strings.Split(nextInput, " ")
				if splitInput[0] == "dir" {
					newNode := Node{
						text:     splitInput[1],
						size:     0,
						children: []*Node{},
						parent: curNode,
					}
					curNode.children = append(curNode.children, &newNode)

				} else {
					val, _ := strconv.Atoi(splitInput[0])
					newNode := Node{
						text:     splitInput[1],
						size:     val,
						children: []*Node{},
						parent: curNode,
					}
					curNode.children = append(curNode.children, &newNode)
				}
			}

		} else if input[0:4] == "$ cd" {
			splitInput := strings.Split(input, " ")
			if splitInput[2] == ".." {
				fmt.Println("Going to parent")
				fmt.Println(curNode.text)
				// Go to parent
				curNode = curNode.parent
			} else {
				// Go to child
				for _, child := range curNode.children {
					if child.text == splitInput[2] {
						curNode = child
						break
					}
				}
			}
		}
	}
	fmt.Println(SIZE_TO_CHECK)
	// fmt.Println(checkSubstr(string(input[0]), input, 0, 4)+1) // Part 1
	// fmt.Println(checkSubstr(string(input[0]), input, 0, 14)+1) // Part 2
}
