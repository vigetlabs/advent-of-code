package translation

import (
  "fmt"
  "testing"
)

func TestGenerateTranslation(t *testing.T) {
  calibratingDigits := [10]string{"abcdefg", "bcdef", "acdfg", "abcdf", "abd", "abcdef", "bcdefg", "abef", "abcdeg", "ab"}
  translation := GenerateTranslation(calibratingDigits)

  check(t, translation["ab"], 1)
  check(t, translation["abef"], 4)
  check(t, translation["abd"], 7)
  check(t, translation["abcdefg"], 8)

  check(t, translation["abcdef"], 9)
  check(t, translation["abcdeg"], 0)
  check(t, translation["bcdefg"], 6)

  check(t, translation["abcdf"], 3)
  check(t, translation["bcdef"], 5)
  check(t, translation["acdfg"], 2)
}

func TestWithLength(t *testing.T) {
  calibratingDigits := [10]string{"abcdefg", "bcdef", "acdfg", "abcdf", "abd", "abcdef", "bcdefg", "abef", "abcdeg", "ab"}

  filtered := withLength(calibratingDigits, 5)
  expected := []string{"bcdef", "acdfg", "abcdf"}

  if (!sliceEqual(filtered, expected)) {
    t.Errorf("Fail!")
    fmt.Println("Got:     ", filtered)
    fmt.Println("Expected:", expected)
  }

  filtered = withLength(calibratingDigits, 6)
  expected = []string{"abcdef", "bcdefg", "abcdeg"}

  if (!sliceEqual(filtered, expected)) {
    t.Errorf("Fail!")
    fmt.Println("Got:     ", filtered)
    fmt.Println("Expected:", expected)
  }
}

func TestDigitFor(t *testing.T) {
  translation := make(map[string]int)
  translation["abef"] = 4
  translation["abd"] = 7

  got := digitFor(translation, 4)
  expected := "abef"

  if got != expected {
    t.Errorf("Fail!")
    fmt.Println("Got:     ", got)
    fmt.Println("Expected:", expected)
  }

  got = digitFor(translation, 7)
  expected = "abd"

  if got != expected {
    t.Errorf("Fail!")
    fmt.Println("Got:     ", got)
    fmt.Println("Expected:", expected)
  }
}

func TestOverlap(t *testing.T) {
  s1 := "abcde"
  s2 := "bdf"
  s3 := "zzz"

  check(t, overlap(s1, s2), 2)
  check(t, overlap(s1, s3), 0)
}

func check(t *testing.T, got int, expected int) {
  if (expected != got) {
    t.Errorf("Fail! Expected %d, got %d", expected, got)
  }
}

func sliceEqual(a []string, b []string) bool {
    if len(a) != len(b) {
        return false
    }
    for i, v := range a {
        if v != b[i] {
            return false
        }
    }
    return true
}
