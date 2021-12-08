package conversion

import (
  // "fmt"
  "testing"
)

func TestGenerateConversion(t *testing.T) {
  calibratingDigits := [10]string{"abcdefg", "bcdef", "acdfg", "abcdf", "abd", "abcdef", "bcdefg", "abef", "abcdeg", "ab"}
  converstion := GenerateConversion(calibratingDigits)
  check(t, converstion["a"], "g")
}

func check(t *testing.T, got string, expected string) {
  if (expected != got) {
    t.Errorf("Fail! Expected %s, got %s", expected, got)
  }
}
