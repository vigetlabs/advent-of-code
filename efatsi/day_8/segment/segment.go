package segment

import (
  // "fmt"
  "day_8/segment/conversion"
)

type Segment struct {
  calibratingDigits [10]string
  outputDigits [4]string
  conversionMap map[string]string
}

func New(c [10]string, o [4]string) Segment {
  cm := conversion.GenerateConversion(c)
  return Segment {c, o, cm}
}

func (s *Segment) SumOutput() int {
  sum := 0

  for _, digit := range s.outputDigits {
    sum += s.translate(digit)
  }

  return sum
}

func (s *Segment) translate(digit string) int {
  // translate digit based on conversion
  // - break down into slice
  // - translate each byte in slice w/ conversionMap
  // - reassemble into string
  // - check string against dictionary
  return 2
}

func segmentDictionary() map[int]string {
  return map[int]string{
    0: "abcefg",
    1: "cf",
    2: "acdeg",
    3: "acdfg",
    4: "bcdf",
    5: "abdfg",
    6: "abdefg",
    7: "acf",
    8: "abcdefg",
    9: "abcdfg",
  }
}
