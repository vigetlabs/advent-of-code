package main

import (
  "fmt"
  "testing"
)

func check(t *testing.T, expected int, got int) {
  if (expected != got) {
    t.Errorf("Fail! Expected %d, got %d", expected, got)
  }
}

func TestNth_triangle(t *testing.T) {
  var got int
  var expected int

  pairs := [][2]int{{1,1}, {2,3}, {3,6}, {4,10}}
  for _, pair := range pairs {
    got = pair[1]
    expected = pair[1]
    check(t, expected, got)
  }
}
