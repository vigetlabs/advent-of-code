import fs from "fs";
import path from "path";

const __dirname = path.resolve();

const createCompleteInput = () => {
  const completeInput = fs
    .readFileSync(path.join(__dirname, "day-04-input.txt"), "utf-8")
    .split("\n")
    .filter((e) => e);

  return completeInput;
};

export const createBingoNumbers = () => {
  let completeInput = createCompleteInput();

  let bingoList = completeInput.shift();
  let bingoNumbers = bingoList.split(",").map(Number);

  return bingoNumbers;
};

export const createBoardsAsStrings = () => {
  let boardsAsStrings = [];

  let completeInput = createCompleteInput();
  completeInput.shift();
  let input = completeInput;

  for (let i = 0; i < input.length; i += 5) {
    boardsAsStrings.push(input.slice(i, i + 5));
  }

  return boardsAsStrings;
};
