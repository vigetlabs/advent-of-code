package translation

import (
  // "fmt"
  "testing"
)

func TestTranslations(t *testing.T) {
  x := 1
  y := 2
  z := 3

  translations := []Translation{
    Translate3, // x, -z, y
    Translate6, // -x, y, -z
  }

  for _, t := range translations {
    x, y, z = t(x, y, z)
  }

  // combining these two gets:
  // -x, -z, -y => -1, -3, -2

  if !(x == -1) {
    t.Errorf("Fail! Expected x == -1, got %d", x)
  }
  if !(y == -3) {
    t.Errorf("Fail! Expected y == -3, got %d", y)
  }
  if !(z == -2) {
    t.Errorf("Fail! Expected z == -2, got %d", z)
  }
}
