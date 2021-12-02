import fs from 'fs'
import path from 'path'

const __dirname = path.resolve()

function main() {
  const input = fs.readFileSync(path.join(__dirname, 'day02/input'), 'utf8')
  const inputArray = input.split('\n')

  let horizontal = 0
  let depth = 0

  const FORWARD = 'forward'
  const UP = 'up'
  const DOWN = 'down'

  inputArray.forEach(value => {
    const splitValue = value.split(' ')
    switch (splitValue[0]) {
      case FORWARD:
        horizontal += parseInt(splitValue[1])
        break
      case UP:
        depth -= parseInt(splitValue[1])
        break
      case DOWN:
        depth += parseInt(splitValue[1])
    }
  })

  console.log(`Forward: ${horizontal}`)
  console.log(`Depth: ${depth}`)
  console.log(`Distance: ${horizontal * depth}`)
}

main()
