package main

import (
  // "fmt"
  "testing"
)

func TestReadLiteralPacket(t *testing.T) {
  inputs := []string{
    "110100101111111000101000",
    "11010001010",
    "0101001000100100",
    "01010000001",
    "10010000010",
    "00110000011",
  }

  expecteds := []int{
    2021,
    10,
    20,
    1,
    2,
    3,
  }

  for i, input := range inputs {
    expected := expecteds[i]
    packet := readLiteralPacket(input)

    if (packet.rawValue != expected) {
      t.Errorf("Fail! Expected %d, got %d", expected, packet.rawValue)
    }
  }
}

func TestReadOperatorPacket0(t *testing.T) {
  input := "00111000000000000110111101000101001010010001001000000000"
  packet := readOperatorPacket(input)

  if (len(packet.subPackets) != 2) {
    t.Errorf("Fail! Expected %d, got %d", 2, len(packet.subPackets))
  }
}

func TestReadOperatorPacket1(t *testing.T) {
  input := "11101110000000001101010000001100100000100011000001100000"
  packet := readOperatorPacket(input)

  if (len(packet.subPackets) != 3) {
    t.Errorf("Fail! Expected %d, got %d", 3, len(packet.subPackets))
  }
}

func TestPacketValue(t *testing.T) {
  inputOutputs := map[string]int{
    "C200B40A82": 3,
    "04005AC33890": 54,
    "880086C3E88112": 7,
    "CE00C43D881120": 9,
    "D8005AC2A8F0": 1,
    "F600BC2D8F": 0,
    "9C005AC2F8F0": 0,
    "9C0141080250320F1802104A08": 1,
  }

  for input, output := range inputOutputs {
    binary := hexToBin(input)
    packet := readPacket(binary)
    got := packet.value()

    if got != output {
      t.Errorf("Fail! Expected %d, got %d", output, got)
    }
  }
}
