package main

import (
  // "fmt"
  "testing"
)

func TestCalculateFuel(t *testing.T) {
  inputs := [][]int{{1,2,3}, {1,2,10}}
  expecteds := []int{2, 9}
  target := 2

  for i, expected := range expecteds {
    input := inputs[i]

    check(t, calculateFuel(input, target), expected)
  }
}

func TestCalculateExpensiveFuel(t *testing.T) {
  inputs := [][]int{{1,2,3}, {1,2,10}}
  expecteds := []int{2, 37}
  target := 2

  for i, expected := range expecteds {
    input := inputs[i]

    check(t, calculateExpensiveFuel(input, target), expected)
  }
}

func TestNthTriangle(t *testing.T) {
  pairs := [][2]int{{1,1}, {2,3}, {3,6}, {4,10}}

  for _, pair := range pairs {
    input := pair[0]
    expected := pair[1]

    check(t, nthTriangle(input), expected)
  }
}

func check(t *testing.T, got int, expected int) {
  if (expected != got) {
    t.Errorf("Fail! Expected %d, got %d", expected, got)
  }
}
