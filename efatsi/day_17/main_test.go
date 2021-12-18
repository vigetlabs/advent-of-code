package main

import (
  "fmt"
  "testing"
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

func check(t *testing.T, expected int, got int) {
  if (expected != got) {
    t.Errorf("Fail! Expected %d, got %d", expected, got)
  }
}
