import fs from "fs";
import path from "path";

const __dirname = path.resolve();

const day03part1 = () => {
  const input = fs
    .readFileSync(path.join(__dirname, "day-03-input.txt"), "utf-8")
    .split("\n")
    .filter((e) => e);

  let arrays = [[], [], [], [], [], [], [], [], [], [], [], []];
  for (let i = 0; i < input.length; i++) {
    for (let j = 0; j < arrays.length; j++) arrays[j].push(input[i][j]);
  }

  let gammaValue = [];
  let epsilonValue = [];

  const findMostCommonElement = (array) => {
    let object = {};

    for (let i = 0; i < array.length; i++) {
      let element = array[i];
      if (object[element] == null) object[element] = 1;
      else object[element]++;
    }

    let value = Math.max(...Object.values(object));
    console.log(Object.values(object));
    gammaValue.push(value.find((key) => object[key] === value));
  };

  arrays.forEach((element) => findMostCommonElement(element));

  const findEpsilonValue = (gammaValue) => {
    for (let i = 0; i < gammaValue.length; i++) {
      if (gammaValue[i] == 0) {
        epsilonValue.push("1");
      } else {
        epsilonValue.push("0");
      }
    }
  };

  findEpsilonValue(gammaValue);

  let gammaDecimal = parseInt(gammaValue.join(""), 2);
  let epsilonDecimal = parseInt(epsilonValue.join(""), 2);
  let answer = gammaDecimal * epsilonDecimal;

  console.log(answer);
};

day03part1();
