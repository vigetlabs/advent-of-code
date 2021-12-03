import fs from 'fs'
import path from 'path'

const __dirname = path.resolve()

function divideArray(array, position) {
  if (array.length === 1) {
    return { mostCommonArray: array, leastCommonArray: array }
  }

  let zero = 0
  let one = 0

  const leastCommonArray = []
  const mostCommonArray = []

  array.forEach(element => {
    element[position] === '0' ? zero++ : one++
  })

  let mostCommon = zero > one ? '0' : '1'

  array.forEach(element => {
    if (element[position] === mostCommon) {
      mostCommonArray.push(element)
    } else {
      leastCommonArray.push(element)
    }
  })

  return { mostCommonArray, leastCommonArray }
}

function main() {
  const input = fs.readFileSync(path.join(__dirname, 'day03/input'), 'utf8')
  const inputArray = input.split('\n')
  const bitLength = inputArray[0].length

  let result = divideArray(inputArray, 0)

  let generator = result.mostCommonArray
  let scrubber = result.leastCommonArray

  for (var i = 1; i < bitLength; i++) {
    generator = divideArray(generator, i).mostCommonArray
    scrubber = divideArray(scrubber, i).leastCommonArray
  }

  generator = parseInt(generator[0], 2)
  scrubber = parseInt(scrubber[0], 2)

  console.log(`O2 Generator Rating: ${generator}`)
  console.log(`CO2 Scrubber Rating: ${scrubber}`)
  console.log(`Life Support Rating: ${generator * scrubber}`)
}

main()
