package main

import (
  "fmt"
  "os"
  "strings"
  "strconv"
)

const headerLength = 6

const debug = true
const filename = "example.txt"

// const debug = false
// const filename = "input.txt"

type Packet struct {
  version int
  packetType string      // "operator" | "literal"
  subPackets [](*Packet) // when "operator"
  value int              // when "literal"
  binary string
  startIndex int
  endIndex int
}

func main() {
  data, _ := os.ReadFile(filename)
  hexInput := strings.Trim(string(data), "\n ")
  binary := hexToBin(hexInput)

  packet := readPacket(binary)

  fmt.Println("packet: ", packet)
}

func readPacket(binary string) Packet {
  packetType := versionFrom(binToInt(binary[3:6]))

  if packetType == "literal" {
    return readLiteralPacket(binary)
  } else {
    // TODO: read operator
    return readLiteralPacket(binary)
  }
}

func readLiteralPacket(binary string) Packet {
  value, bodyLength := walkOverLiteral(binary)
  endIndex := 0 + headerLength + bodyLength

  packet := Packet {
    version: binToInt(binary[0:3]),
    packetType: "literal",
    value: value,
    binary: binary[0:endIndex],
    startIndex: 0,
    endIndex: endIndex,
  }

  // if debug {
  //   fmt.Println("packet: ", packet)
  // }

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

// Thanks internet: https://forum.golangbridge.org/t/hex-to-binary-function/4560/2
func hexToBin(hex string) string {
  ui, _ := strconv.ParseUint(hex, 16, 64)
  // %016b indicates base 2, zero padded, with 16 characters
  return fmt.Sprintf("%016b", ui)
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
