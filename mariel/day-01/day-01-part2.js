import fs from "fs";
import path from "path";

const __dirname = path.resolve();

const day01part2 = () => {
  const input = fs
    .readFileSync(path.join(__dirname, "day-01-input.txt"), "utf-8")
    .split("\n")
    .map(Number);

  let count = 0;

  for (let i = 0; i < input.length; i++) {
    if (
      input.slice(i, i + 3).reduce((a, b) => a + b) <
      input.slice(i + 1, i + 4).reduce((a, b) => a + b, 0)
    ) {
      count++;
    }
  }

  console.log(count);
};

day01part2();
