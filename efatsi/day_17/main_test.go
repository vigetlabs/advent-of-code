package main

import (
  // "fmt"
  "testing"
  "math"
)

func TestShotsWithStep1(t *testing.T) {
  expected := 66
  got := shotsWithStep(1)
  check(t, expected, got)
}

func TestShotsWithStep2(t *testing.T) {
  expected := 15
  got := shotsWithStep(2)
  check(t, expected, got)
}

func TestShotsWithStep3(t *testing.T) {
  expected := 8
  got := shotsWithStep(3)
  check(t, expected, got)
}

func TestShotsWithStep9(t *testing.T) {
  expected := 2
  got := shotsWithStep(9)
  check(t, expected, got)
}

func TestShotsWithStep10(t *testing.T) {
  expected := 2
  got := shotsWithStep(10)
  check(t, expected, got)
}

func TestSolveQuadratic20(t *testing.T) {
  expected := 5.84
  got := round(solveQuadratic(20))

  checkF(t, expected, got)
}

func TestAddToZero20(t *testing.T) {
  expected := 210
  got := addToZero(20)

  check(t, expected, got)
}

func TestAddToZeroSolveQuadratic(t *testing.T) {
  expected := 20.0
  got := solveQuadratic(addToZero(20))
  checkF(t, expected, got)

  expectedInt := 210
  gotInt := addToZero(int(solveQuadratic(210)))
  check(t, expectedInt, gotInt)
}

func check(t *testing.T, expected int, got int) {
  if (expected != got) {
    t.Errorf("Fail! Expected %d, got %d", expected, got)
  }
}

func checkF(t *testing.T, expected float64, got float64) {
  if (expected != got) {
    t.Errorf("Fail! Expected %f, got %f", expected, got)
  }
}

func round(x float64) float64 {
  return math.Round(x*100)/100
}
