import { createBoardsAsStrings } from "./createInput.js";

let boardsAsStrings = createBoardsAsStrings();

const createRows = (boardsAsStrings) => {
  let boardRows = [];
  let rows = [];

  for (let i = 0; i < boardsAsStrings.length; i++) {
    let boardFromList = boardsAsStrings[i];

    //and for each string inside that board
    for (let j = 0; j < boardFromList.length; j++) {
      //turn that string into an array of numbers
      let newRow = boardFromList[j]
        .split(" ")
        .filter((e) => e)
        .map(Number);

      //add that array to an empty board
      boardRows.push(newRow);
    }

    //reset board to be empty and start the process over
    rows.push(boardRows);
    boardRows = [];
  }

  return rows;
};
const rows = createRows(boardsAsStrings);

const createColumns = (rows) => {
  let columns = [];
  for (let i = 0; i < rows.length; i++) {
    let board = rows[i];

    columns.push(board.map((_, i) => board.map((row) => row[i])));
  }

  return columns;
};
const columns = createColumns(rows);

export const boards = [];
let board = {};

const makeBoardObject = (rows, columns) => {
  for (let i = 0; i < rows.length; i++) {
    let boardRows = rows[i];
    let boardColumns = columns[i];

    board.number = i;
    board.rows = boardRows;
    board.columns = boardColumns;

    boards.push(board);

    board = {};
  }
};

makeBoardObject(rows, columns);
