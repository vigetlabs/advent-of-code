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


type boardSquare struct {
	num int64
	marked bool
}

func removeEmpty(array []string) []string {
	var newArray []string
	for _, element := range array {
		if element != "" {
			newArray = append(newArray, element)
		}
	}
	return newArray
}

func createBoardArray(block []string) [5][5]boardSquare {
	boardArray := [5][5]boardSquare{}
	for i :=0; i < 5; i++ {
		currentLine := removeEmpty(strings.Split(block[i], " "))
		for j := 0; j < 5; j++ {
				num, _ := strconv.ParseInt(currentLine[j], 10, 64)
				boardArray[i][j].num = num
				boardArray[i][j].marked = false
		}
	}
	return boardArray
}

func checkQuintuplet(quintuplet [5]boardSquare) bool {
	return quintuplet[0].marked && quintuplet[1].marked && quintuplet[2].marked && quintuplet[3].marked && quintuplet[4].marked
}

func checkBoard(board [5][5]boardSquare) bool{
	// Check horizontal
	for i := 0; i < 5; i++ {
		if checkQuintuplet(board[i]) {
			return true
		}
	}
	// Check vertical
	for i := 0; i < 5; i++ {
		vertical := [5]boardSquare{board[0][i], board[1][i], board[2][i], board[3][i], board[4][i]}
		if checkQuintuplet(vertical) {
			return true
		}
	}
	// Check diagonals
	diagonal1 := [5]boardSquare{board[0][0], board[1][1], board[2][2], board[3][3], board[4][4]}
	diagonal2 := [5]boardSquare{board[0][4], board[1][3], board[2][2], board[3][1], board[4][0]}
	if checkQuintuplet(diagonal1) || checkQuintuplet(diagonal2) {
		return true
	}
	return false
}

func getWinner(boardsArray [][5][5]boardSquare) ([5][5]boardSquare, error) {
	for _, board := range boardsArray {
		if checkBoard(board) {
			return board, nil
		}
	}
	return [5][5]boardSquare{}, errors.New("no winner")
}

func getWinners(boardsArray [][5][5]boardSquare) [][5][5]boardSquare {
	var winners [][5][5]boardSquare
	for _, board := range boardsArray {
		if checkBoard(board) {
			winners = append(winners, board)
		}
	}
	return winners
}


func nextBingoValue(boardsArray [][5][5]boardSquare, input int64) [][5][5]boardSquare{
	newBoardsArray := [][5][5]boardSquare{}
	for i := 0; i < len(boardsArray); i++ {
		board := boardsArray[i]
		newBoard := [5][5]boardSquare{}
		for j := 0; j < 5; j++ {
			for k := 0; k < 5; k++ {
				if board[j][k].num == input {
					newBoard[j][k].num = input
					newBoard[j][k].marked = true
				} else {
					newBoard[j][k].num = board[j][k].num
					newBoard[j][k].marked = board[j][k].marked

				}
			}
		}
		newBoardsArray = append(newBoardsArray, newBoard)
	}
	return newBoardsArray
}

func getSumUnmarked(board [5][5]boardSquare) int64 {
	sum := int64(0)
	for i := 0; i < 5; i++ {
		for j := 0; j < 5; j++ {
			if !board[i][j].marked {
				sum += board[i][j].num
			}
		}
	}
	return sum
}

func removeWinner(boardsArray [][5][5]boardSquare, winner [5][5]boardSquare) [][5][5]boardSquare {
	var newBoardsArray [][5][5]boardSquare
	for i := 0; i < len(boardsArray); i++ {
		if boardsArray[i] != winner {
			newBoardsArray = append(newBoardsArray, boardsArray[i])
		}
	}
	return newBoardsArray
}

func copyBoardsArray(boardsArray [][5][5]boardSquare) [][5][5]boardSquare {
	var newBoardsArray [][5][5]boardSquare
	for i := 0; i < len(boardsArray); i++ {
			newBoardsArray = append(newBoardsArray, boardsArray[i])
	}
	return newBoardsArray
}

func removeWinners(boardsArray [][5][5]boardSquare) [][5][5]boardSquare {
	newBoardsArray := copyBoardsArray(boardsArray)

	winners := getWinners(boardsArray)
	for _, winner := range winners {

		newBoardsArray = removeWinner(newBoardsArray, winner)
	}
	return newBoardsArray
}

func day4_part1(input []string, boardsArray [][5][5]boardSquare) int64{
	winner, boardErr := getWinner(boardsArray)
	i := 0
	for boardErr != nil {
		parsedInput, _ := strconv.ParseInt(input[i], 10, 64)
		boardsArray = nextBingoValue(boardsArray, parsedInput)
		winner, boardErr = getWinner(boardsArray)
		i++
	}

	sumUnmarked := getSumUnmarked(winner)
	winningInput, _ := strconv.ParseInt(input[i-1], 10, 64)
	return sumUnmarked * winningInput
}


func day4_part2(input []string, boardsArray [][5][5]boardSquare) int64{
	winner, boardErr := getWinner(boardsArray)
	var currentWinner [5][5]boardSquare
	var winningInput int64
	i := 0
	for i < len(input) {
		parsedInput, _ := strconv.ParseInt(input[i], 10, 64)
		boardsArray = nextBingoValue(boardsArray, parsedInput)
		winner, boardErr = getWinner(boardsArray)
		if boardErr == nil {
			currentWinner = winner
			winningInput = parsedInput
			boardsArray = removeWinners(boardsArray)
		}
		i++
	}
	sumUnmarked := getSumUnmarked(currentWinner)
	return sumUnmarked * winningInput
}

func main() {
	dat, err := os.Open("../input/day4.txt")
	check(err)
	defer dat.Close()
	scanner := bufio.NewScanner(dat)
	scanner.Split(bufio.ScanLines)
	var input []string
	block := []string{}
	var boardsArray [][5][5]boardSquare
	i := 0
	for scanner.Scan() {
		if i == 0 {
			firstLine := scanner.Text()
			input = strings.Split(firstLine, ",")
			i++
		} else {
			l := scanner.Text()

			if len(strings.TrimSpace(l)) != 0 {
					block = append(block, l)
					continue
			}

			// At this point, the script has reached an empty line,
			// which means the block is ready to be processed.
			// If the block is not empty, append it to the buffer and empty it.
			if len(block) != 0 {
				currentBoard := createBoardArray(block)
				boardsArray = append(boardsArray, currentBoard)
				block = []string{}
			}

		}
	}

	copiedBoardsArray := copyBoardsArray(boardsArray)
	fmt.Println(day4_part1(input, boardsArray))
	fmt.Println(day4_part2(input, copiedBoardsArray))
}


