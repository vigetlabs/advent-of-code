import { createBingoNumbers } from "./createInput.js";
import { boards } from "./createBoards.js";

const day04part1 = () => {
  let bingoNumbers = createBingoNumbers();

  const markBoard = (board, bingoNumber) => {
    let myBoard = board;

    for (let i = 0; i < 5; i++) {
      if (myBoard.rows[i].includes(bingoNumber)) {
        let index = myBoard.rows[i].indexOf(bingoNumber);

        myBoard.rows[i][index] = "X";
      }

      if (myBoard.columns[i].includes(bingoNumber)) {
        let index = myBoard.columns[i].indexOf(bingoNumber);
        myBoard.columns[i][index] = "X";
      }
    }

    return myBoard;
  };

  const findWinner = () => {
    let myBoards = boards;

    for (let i = 0; i < bingoNumbers.length; i++) {
      for (let j = 0; j < myBoards.length; j++) {
        let board = markBoard(myBoards[j], bingoNumbers[i]);
        for (let k = 0; k < 5; k++) {
          let winner = ["X", "X", "X", "X", "X"];
          let row = board.rows[k];
          let column = board.columns[k];
          if (
            winner.every((val, index) => val === row[index]) ||
            winner.every((val, index) => val === column[index])
          ) {
            let answer = bingoNumbers[i] * sumBoard(board);

            return console.log(answer);
          }
        }
      }
    }
  };

  const findLoser = () => {
    let loserBoards = boards;
    let winners = [];

    for (let i = 0; i < bingoNumbers.length; i++) {
      for (let j = 0; j < loserBoards.length; j++) {
        let board = markBoard(loserBoards[j], bingoNumbers[i]);

        for (let k = 0; k < 5; k++) {
          let winner = ["X", "X", "X", "X", "X"];
          let row = board.rows[k];
          let column = board.columns[k];
          if (
            winner.every((val, index) => val === row[index]) ||
            winner.every((val, index) => val === column[index])
          ) {
            if (!winners.includes(board)) {
              winners.push(board);
            }

            if (winners.length == 100) {
              return console.log(
                bingoNumbers[i] * sumBoard(winners[winners.length - 1])
              );
            }
          }
        }
      }
    }
  };

  const sumBoard = (board) => {
    let sum = 0;

    for (let i = 0; i < 5; i++) {
      let row = board.rows[i];

      for (let j = 0; j < 5; j++) {
        if (typeof row[j] == "number") {
          sum += row[j];
        }
      }
    }

    return sum;
  };

  findWinner();
  findLoser();
};

day04part1();
