package segment

import (
  // "fmt"
  "strconv"
  "day_8/segment/translation"
)

type Segment struct {
  calibratingDigits [10]string
  outputDigits [4]string
  translation map[string]int
}

func New(cd [10]string, od [4]string) Segment {
  tr := translation.GenerateTranslation(cd)
  return Segment {cd, od, tr}
}

func (s *Segment) AssembleOutput() int {
  number := ""

  for _, digit := range s.outputDigits {
    number += strconv.Itoa(s.translate(digit))
  }

  val, _ := strconv.Atoi(number)
  return val
}

func (s *Segment) translate(digit string) int {
  return s.translation[digit]
}
