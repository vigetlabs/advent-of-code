import fs from "fs";
import path from "path";

const __dirname = path.resolve();

const day02part02 = () => {
  const input = fs
    .readFileSync(path.join(__dirname, "day2-input.txt"), "utf-8")
    .split("\n");

  let aim = 0;
  let horizontalPosition = 0;
  let depth = 0;

  for (let i = 0; i < input.length; i++) {
    let firstLetter = input[i].charAt(0);

    switch (firstLetter) {
      case "f":
        horizontalPosition += Number(input[i].charAt(8));
        depth += Number(input[i].charAt(8)) * aim;
        break;
      case "d":
        aim += Number(input[i].charAt(5));
        break;
      case "u":
        aim -= Number(input[i].charAt(3));
        break;
    }
  }

  let output = depth * horizontalPosition;

  console.log(output);
};

day02part02();
