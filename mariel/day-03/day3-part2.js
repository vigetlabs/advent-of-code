import fs from "fs";
import path from "path";

const __dirname = path.resolve();

const day03part2 = () => {
  const inputArray = fs
    .readFileSync(path.join(__dirname, "day-03-input.txt"), "utf-8")
    .split("\n")
    .filter((e) => e);

  let mostAnswer;
  let leastAnswer;

  let findMostCommonElement = (input) => {
    let myInput = input;
    let key;
    let zero = 0;
    let one = 0;

    for (let j = 0; j < 12; j++) {
      for (let i = 0; i < myInput.length; i++) {
        let row = myInput[i];

        if (row[j] == "0") {
          zero += 1;
        } else {
          one += 1;
        }
      }

      if (zero > one) {
        key = 0;
      } else {
        key = 1;
      }

      myInput = myInput.filter((element) => element[j] == key);
      zero = 0;
      one = 0;
    }

    mostAnswer = myInput[0];
    return mostAnswer;
  };

  findMostCommonElement(inputArray);

  let findLeastCommonElement = (input) => {
    let myInput = input;
    let zero = 0;
    let one = 0;
    let key;

    for (let j = 0; j < 12; j++) {
      for (let i = 0; i < myInput.length; i++) {
        let row = myInput[i];

        if (row[j] == "0") {
          zero += 1;
        } else one += 1;
      }

      if (zero < one) {
        key = 0;
      } else if (zero == one) {
        key = 0;
      } else key = 1;

      if (myInput.length > 1) {
        myInput = myInput.filter((element) => element[j] == key);
        zero = 0;
        one = 0;
      }
    }
    leastAnswer = myInput[0];
    return leastAnswer;
  };

  findLeastCommonElement(inputArray);

  let mostDecimal = parseInt(mostAnswer, 2);
  let leastDecimal = parseInt(leastAnswer, 2);
  let answer = mostDecimal * leastDecimal;

  console.log(answer);
};

day03part2();
