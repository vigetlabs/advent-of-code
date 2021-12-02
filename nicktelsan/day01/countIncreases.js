import fs from 'fs'
import path from 'path'

const __dirname = path.resolve()

function main() {
  const input = fs.readFileSync(path.join(__dirname, 'input'), 'utf8')
  const inputArray = input.split('\n')

  let increases = 0
  let prev = null

  inputArray.forEach(value => {
    const parsedValue = parseInt(value)

    if (prev && parsedValue > prev) {
      increases++
    }
    prev = parsedValue
  })
  console.log(increases)
}

main()
