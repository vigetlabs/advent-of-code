package translation

import (
  // "fmt"
  "strings"
)

func GenerateTranslation(calibratingDigits [10]string) map[string]int {
  translation := make(map[string]int)

  // Grab easy ones
  for _, digit := range calibratingDigits {
    switch len(digit) {
    case 2:
        translation[digit] = 1
    case 3:
        translation[digit] = 7
    case 4:
        translation[digit] = 4
    case 7:
        translation[digit] = 8
    }
  }

  // Determine digits w/ length 6
  for _, digit := range withLength(calibratingDigits, 6) {
    if overlap(digit, digitFor(translation, 4)) == 4 {
      translation[digit] = 9
    } else if overlap(digit, digitFor(translation, 1)) == 2 {
      translation[digit] = 0
    } else {
      translation[digit] = 6
    }
  }

  // Determine digits w/ length 5
  for _, digit := range withLength(calibratingDigits, 5) {
    if overlap(digit, digitFor(translation, 7)) == 3 {
      translation[digit] = 3
    } else if overlap(digit, digitFor(translation, 6)) == 5 {
      translation[digit] = 5
    } else {
      translation[digit] = 2
    }
  }

  return translation
}

func withLength(calibratingDigits [10]string, length int) []string {
  filtered := make([]string, 0)

  for _, digit := range calibratingDigits {
    if len(digit) == length {
      filtered = append(filtered, digit)
    }
  }

  return filtered
}

func digitFor(translation map[string]int, value int) string {
  for k, v := range translation {
    if v == value {
      return k
    }
  }

  return ""
}

func overlap(s1 string, s2 string) int {
  sl1 := strings.Split(s1, "")
  sl2 := strings.Split(s2, "")

  sum := 0
  for _, x := range sl1 {
    for _, y := range sl2 {
      if x == y {
        sum++
      }
    }
  }

  return sum
}
