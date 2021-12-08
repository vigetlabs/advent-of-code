import fs from "fs";
import path from "path";

const __dirname = path.resolve();

const day02part01 = () => {
  const input = fs
    .readFileSync(path.join(__dirname, "day2-input.txt"), "utf-8")
    .split("\n");

  let forwardArray = [];
  let downUpArray = [];

  // need to split apart forward, down, and up
  // forward needs to be an array of positive numbers
  // downUp needs to be an array of positive and negative numbers
  for (let i = 0; i < input.length; i++) {
    if (input[i].includes("forward")) {
      let inputForwardString = input[i].replace("forward", "");
      let inputNumber = Number(inputForwardString);
      forwardArray.push(inputNumber);
    } else {
      if (input[i].includes("down")) {
        let inputDownString = input[i].replace("down", "");
        let inputDownNumber = Number(inputDownString);
        downUpArray.push(inputDownNumber);
      }

      if (input[i].includes("up")) {
        let inputUpString = input[i].replace("up ", "-");
        let inputUpNumber = parseInt(inputUpString);
        downUpArray.push(inputUpNumber);
      }
    }
  }

  // add the elements of each array together
  let sumDownUpArray = downUpArray.reduce((a, b) => a + b, 0);
  let sumForwardArray = forwardArray.reduce((a, b) => a + b, 0);

  let answer = sumDownUpArray * sumForwardArray;
  console.log(answer);
};

day02part01();
