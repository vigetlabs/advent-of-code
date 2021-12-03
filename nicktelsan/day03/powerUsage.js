import fs from 'fs'
import path from 'path'

const __dirname = path.resolve()

function mostCommonBit(array, position) {
  let zero = 0
  let one = 0

  array.forEach(element => {
    element[position] === '0' ? zero++ : one++
  })

  return zero > one ? '0' : '1'
}

function flipBits(number) {
  let newNumber = ''

  for (var i = 0; i < number.length; i++) {
    newNumber += number[i] === '0' ? '1' : '0'
  }
  return newNumber
}

function main() {
  const input = fs.readFileSync(path.join(__dirname, 'day03/input'), 'utf8')
  const inputArray = input.split('\n')
  const bitLength = inputArray[0].length

  let gamma = ''

  for (var i = 0; i < bitLength; i++) {
    gamma += mostCommonBit(inputArray, i)
  }

  let epsilon = flipBits(gamma)

  console.log(gamma)
  gamma = parseInt(gamma, 2)
  epsilon = parseInt(epsilon, 2)

  console.log(`gamma: ${gamma}`)
  console.log(`epsilon: ${epsilon}`)
  console.log(`Power: ${gamma * epsilon}`)
}

main()
