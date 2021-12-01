import fs from 'fs'
import path from 'path'

const __dirname = path.resolve()

function main() {
  const input = fs.readFileSync(path.join(__dirname, 'day01/input'), 'utf8')
  const inputArray = input.split('\n')

  const maxLength = inputArray.length - 2

  let increases = 0
  let prev = null

  for (let i = 0; i < maxLength; i++) {
    const sum =
      parseInt(inputArray[i]) +
      parseInt(inputArray[i + 1]) +
      parseInt(inputArray[i + 2])

    if (prev && sum > prev) {
      increases++
    }

    prev = sum
  }

  console.log(increases)
}

main()
