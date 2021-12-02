import fs from "fs";
import path from "path";

const __dirname = path.resolve();

const day01 = () => {
  const input = fs.readFileSync(path.join(__dirname, "input.txt"), "utf-8");
  console.log(input);
};

day01();
