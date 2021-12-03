import fs from "fs";
import path from "path";

const __dirname = path.resolve();

const day01 = () => {
  const input = fs
    .readFileSync(path.join(__dirname, "day-01-input.txt"), "utf-8")
    .split("\n")
    .map(Number);

  let count = 0;

  for (let i = 0; i < input.length; i++) {
    if (input[i] > input[i - 1]) {
      count++;
    }
  }

  console.log(count);
};

day01();
