package main

import (
	"bufio"
	"container/list"
	"fmt"
	"os"
	"strconv"
	"strings"
)

type customStack struct {
    stack *list.List
}

func (c *customStack) Push(value string) {
    c.stack.PushFront(value)
}

func (c *customStack) Pop() error {
    if c.stack.Len() > 0 {
        ele := c.stack.Front()
        c.stack.Remove(ele)
    }
    return fmt.Errorf("Pop Error: Stack is empty")
}

func (c *customStack) Front() (string, error) {
    if c.stack.Len() > 0 {
        if val, ok := c.stack.Front().Value.(string); ok {
            return val, nil
        }
        return "", fmt.Errorf("Peep Error: Stack Datatype is incorrect")
    }
    return "", fmt.Errorf("Peep Error: Stack is empty")
}

func (c *customStack) Size() int {
    return c.stack.Len()
}

func (c *customStack) Empty() bool {
    return c.stack.Len() == 0
}

func check(e error) {
	if e != nil {
		panic(e)
	}
}

func createStacks() []*customStack {
	stacks := []*customStack{
		&customStack{
			stack: list.New(),
		},
		&customStack{
			stack: list.New(),
		},
		&customStack{
			stack: list.New(),
		},
		&customStack{
			stack: list.New(),
		},
		&customStack{
			stack: list.New(),
		},
		&customStack{
			stack: list.New(),
		},
		&customStack{
			stack: list.New(),
		},
		&customStack{
			stack: list.New(),
		},
		&customStack{
			stack: list.New(),
		},

	}
	return stacks
}

func addRowToStacks(stacks []*customStack, row string) {
	//                    * [] * haha looks like a face
	for i := 0; i <= 32; i+=4 {
		var subsection string
		if i == 32 {
			subsection = row[i:]
		} else {
			subsection = row[i:i+4]
		}
		trimmed := strings.TrimSpace(subsection)
		if trimmed == "" {
			continue
		} else {
			stacks[i/4].Push(trimmed)
		}
	}

}

func main() {
	dat, err := os.Open("dannybrown/2022/input/day5.txt")
	check(err)
	defer dat.Close()
	scanner := bufio.NewScanner(dat)
	scanner.Split(bufio.ScanLines)
	// Get initial configuration
	scanner.Scan()
	row8 := scanner.Text()
	scanner.Scan()
	row7 := scanner.Text()
	scanner.Scan()
	row6 := scanner.Text()
	scanner.Scan()
	row5 := scanner.Text()
	scanner.Scan()
	row4 := scanner.Text()
	scanner.Scan()
	row3 := scanner.Text()
	scanner.Scan()
	row2 := scanner.Text()
	scanner.Scan()
	row1 := scanner.Text()
	scanner.Scan()
	scanner.Scan()

	stacks := createStacks()
	addRowToStacks(stacks, row1)
	addRowToStacks(stacks, row2)
	addRowToStacks(stacks, row3)
	addRowToStacks(stacks, row4)
	addRowToStacks(stacks, row5)
	addRowToStacks(stacks, row6)
	addRowToStacks(stacks, row7)
	addRowToStacks(stacks, row8)

	isPart2 := true
	for scanner.Scan() {
		instruction := scanner.Text()
		splitInstruction := strings.Split(instruction, " ")
		numberToMove, _ := strconv.Atoi(splitInstruction[1])
		origin, _ := strconv.Atoi(splitInstruction[3])
		destination, _ := strconv.Atoi(splitInstruction[5])

		originStack := stacks[origin-1]
		destinationStack := stacks[destination-1]

		if isPart2 {
			intermediaryStack := &customStack{
				stack: list.New(),
			}
			for i := 0; i < numberToMove; i++ {
				boxToMove, _ := originStack.Front()
				originStack.Pop()
				intermediaryStack.Push(boxToMove)
			}
			// flip the stack over to mimic items being moved out at once
			for i := 0; i < numberToMove; i++ {
				boxToMove, _ := intermediaryStack.Front()
				intermediaryStack.Pop()
				destinationStack.Push(boxToMove)
			}

		} else {
			for i := 0; i < numberToMove; i++ {
				boxToMove, _ := originStack.Front()
				originStack.Pop()
				destinationStack.Push(boxToMove)
			}

		}
	}
	answer := ""
	for _, stack := range stacks {
		front, _ := stack.Front()
		answer += front
	}
	fmt.Println(answer)
}
