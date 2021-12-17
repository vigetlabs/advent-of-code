package main

import (
  "fmt"
  "os"
  "strings"
  "strconv"
)

const headerLength = 6

// const debug = true
const debug = false

const readingHex = true
// const readingHex = false

// const filename = "example.txt"
// const filename = "operator_binary.txt"
const filename = "input.txt"

type Packet struct {
  version int
  packetType string      // "operator" | "literal"
  subPackets [](*Packet) // when "operator"
  value int              // when "literal"
  binary string
  length int
}

func main() {
  data, _ := os.ReadFile(filename)
  hexInput := strings.Trim(string(data), "\n ")

  var binary string
  if readingHex {
    binary = hexToBin(hexInput)
  } else {
    binary = string(hexInput)
  }

  packet := readPacket(binary)

  solvePartOne(packet)
}

func solvePartOne(packet Packet) {
  sum := 0

  countVersion(&sum, packet)

  fmt.Println("packet sum: ", sum)
}

func countVersion(sum *int, packet Packet) {
  *sum += packet.version

  for _, packet := range packet.subPackets {
    countVersion(sum, *packet)
  }
}

func readPacket(binary string) Packet {
  packetType := versionFrom(binToInt(binary[3:6]))

  if packetType == "literal" {
    return readLiteralPacket(binary)
  } else {
    return readOperatorPacket(binary)
  }
}

func readOperatorPacket(binary string) Packet {
  lengthType := binary[6:7]

  operatorPacket := Packet {
    version: binToInt(binary[0:3]),
    packetType: "operator",
  }

  packetLength := 0
  if lengthType == "0" {
    // read next 15 bits [7:7+15]
    // this is length of bits to read into until subpackets are done
    bitCount := binToInt(binary[7:7+15])

    readOffset := 0
    for (readOffset < bitCount) {
      newPacket := readPacket(binary[7+15+readOffset:7+15+bitCount])

      operatorPacket.subPackets = append(operatorPacket.subPackets, &newPacket)
      readOffset += newPacket.length
    }

    packetLength = 7+15+readOffset
  } else {
    // read next 11 bits [7:7+11]
    // this is number of subpackets
    packetCount := binToInt(binary[7:7+11])

    readOffset := 0
    for i := 0; i < packetCount; i++ {
      newPacket := readPacket(binary[7+11+readOffset:])

      operatorPacket.subPackets = append(operatorPacket.subPackets, &newPacket)
      readOffset += newPacket.length
    }

    packetLength = 7+11+readOffset
  }

  operatorPacket.binary = binary[0:packetLength]
  operatorPacket.length = packetLength

  return operatorPacket
}

func readLiteralPacket(binary string) Packet {
  value, bodyLength := walkOverLiteral(binary)
  packetLength := headerLength + bodyLength

  packet := Packet {
    version: binToInt(binary[0:3]),
    packetType: "literal",
    value: value,
    binary: binary[0:packetLength],
    length: packetLength,
  }

  return packet
}

func walkOverLiteral(binary string) (value int, length int) {
  onLastBit := false
  step := 0
  valueString := ""

  for (onLastBit == false) {
    nextStart := headerLength + (step * 5)
    nextBits := binary[nextStart:nextStart+5]

    if nextBits[0] == '0' {
      onLastBit = true
    }

    valueString += nextBits[1:]

    step++
  }

  return binToInt(valueString), step * 5
}



// HELPERS
var HEXMAP = map[string]string {
  "0": "0000",
  "1": "0001",
  "2": "0010",
  "3": "0011",
  "4": "0100",
  "5": "0101",
  "6": "0110",
  "7": "0111",
  "8": "1000",
  "9": "1001",
  "A": "1010",
  "B": "1011",
  "C": "1100",
  "D": "1101",
  "E": "1110",
  "F": "1111",
}

func hexToBin(hex string) string {
  hexes := strings.Split(hex, "")
  var binaryString string
  for _, char := range hexes {
    binaryString += HEXMAP[char]
  }

  return binaryString
}

func binToInt(binary string) int {
  val, _ := strconv.ParseInt(binary, 2, 64)
  return int(val)
}

func versionFrom(key int) string {
  if key == 4 {
    return "literal"
  } else {
    return "operator"
  }
}
