package main

import (
  // "fmt"
  "testing"
)

func TestParse1(t *testing.T) {
  input := "[1,1]"
  unit := parse(input)

  if !(unit.Children[0].UnitType == "literal") {
    t.Errorf("Fail unit.Children[0].UnitType -- Expected %s, got %s", "literal", unit.Children[0].UnitType)
  }
  if !(unit.Children[0].Value == 1) {
    t.Errorf("Fail unit.Children[0].Value -- Expected %d, got %d", 1, unit.Children[0].Value)
  }
  if !(unit.Children[1].UnitType == "literal") {
    t.Errorf("Fail unit.Children[1].UnitType -- Expected %s, got %s", "literal", unit.Children[1].UnitType)
  }
  if !(unit.Children[1].Value == 1) {
    t.Errorf("Fail unit.Children[1].Value -- Expected %d, got %d", 1, unit.Children[1].Value)
  }
}

func TestParse2(t *testing.T) {
  input := "[2,2]"
  unit := parse(input)

  if !(unit.Children[0].UnitType == "literal") {
    t.Errorf("Fail unit.Children[0].UnitType -- Expected %s, got %s", "literal", unit.Children[0].UnitType)
  }
  if !(unit.Children[0].Value == 2) {
    t.Errorf("Fail unit.Children[0].Value -- Expected %d, got %d", 2, unit.Children[0].Value)
  }
  if !(unit.Children[1].UnitType == "literal") {
    t.Errorf("Fail unit.Children[1].UnitType -- Expected %s, got %s", "literal", unit.Children[1].UnitType)
  }
  if !(unit.Children[1].Value == 2) {
    t.Errorf("Fail unit.Children[1].Value -- Expected %d, got %d", 2, unit.Children[1].Value)
  }
}

func TestParse20(t *testing.T) {
  input := "[20,20]"
  unit := parse(input)

  if !(unit.Children[0].UnitType == "literal") {
    t.Errorf("Fail unit.Children[0].UnitType -- Expected %s, got %s", "literal", unit.Children[0].UnitType)
  }
  if !(unit.Children[0].Value == 20) {
    t.Errorf("Fail unit.Children[0].Value -- Expected %d, got %d", 20, unit.Children[0].Value)
  }
  if !(unit.Children[1].UnitType == "literal") {
    t.Errorf("Fail unit.Children[1].UnitType -- Expected %s, got %s", "literal", unit.Children[1].UnitType)
  }
  if !(unit.Children[1].Value == 20) {
    t.Errorf("Fail unit.Children[1].Value -- Expected %d, got %d", 20, unit.Children[1].Value)
  }
}

func TestParseNestFirst(t *testing.T) {
  input := "[[1,1],2]"
  unit := parse(input)

  if !(unit.Children[0].UnitType == "pair") {
    t.Errorf("Fail unit.Children[0].UnitType -- Expected %s, got %s", "pair", unit.Children[0].UnitType)
  }
    if !(unit.Children[0].Children[0].UnitType == "literal") {
      t.Errorf("Fail unit.Children[0].Children[0].UnitType -- Expected %s, got %s", "literal", unit.Children[0].Children[0].UnitType)
    }
    if !(unit.Children[0].Children[0].Value == 1) {
      t.Errorf("Fail unit.Children[0].Children[0].Value -- Expected %d, got %d", 1, unit.Children[0].Children[0].Value)
    }
    if !(unit.Children[0].Children[1].UnitType == "literal") {
      t.Errorf("Fail unit.Children[0].Children[1].UnitType -- Expected %s, got %s", "literal", unit.Children[0].Children[1].UnitType)
    }
    if !(unit.Children[0].Children[1].Value == 1) {
      t.Errorf("Fail unit.Children[0].Children[1].Value -- Expected %d, got %d", 1, unit.Children[0].Children[1].Value)
    }

    if !(unit.Children[1].UnitType == "literal") {
      t.Errorf("Fail unit.Children[1].UnitType -- Expected %s, got %s", "literal", unit.Children[1].UnitType)
    }
    if !(unit.Children[1].Value == 2) {
      t.Errorf("Fail unit.Children[1].Value -- Expected %d, got %d", 2, unit.Children[1].Value)
    }

}

func TestParseNestSecond(t *testing.T) {
  input := "[1,[2,2]]"
  unit := parse(input)

  if !(unit.Children[0].UnitType == "literal") {
    t.Errorf("Fail unit.Children[0].UnitType -- Expected %s, got %s", "literal", unit.Children[0].UnitType)
  }
  if !(unit.Children[0].Value == 1) {
    t.Errorf("Fail unit.Children[0].Value -- Expected %d, got %d", 1, unit.Children[0].Value)
  }

  if !(unit.Children[1].UnitType == "pair") {
    t.Errorf("Fail unit.Children[1].UnitType -- Expected %s, got %s", "pair", unit.Children[1].UnitType)
  }
    if !(unit.Children[1].Children[0].UnitType == "literal") {
      t.Errorf("Fail unit.Children[1].Children[0].UnitType -- Expected %s, got %s", "literal", unit.Children[1].Children[0].UnitType)
    }
    if !(unit.Children[1].Children[0].Value == 2) {
      t.Errorf("Fail unit.Children[1].Children[0].Value -- Expected %d, got %d", 2, unit.Children[1].Children[0].Value)
    }
    if !(unit.Children[1].Children[1].UnitType == "literal") {
      t.Errorf("Fail unit.Children[1].Children[1].UnitType -- Expected %s, got %s", "literal", unit.Children[1].Children[1].UnitType)
    }
    if !(unit.Children[1].Children[1].Value == 2) {
      t.Errorf("Fail unit.Children[1].Children[1].Value -- Expected %d, got %d", 2, unit.Children[1].Children[1].Value)
    }
}

func TestParseNestDouble(t *testing.T) {
  input := "[[1,1],[2,2]]"
  unit := parse(input)

  if !(unit.Children[0].UnitType == "pair") {
    t.Errorf("Fail unit.Children[0].UnitType -- Expected %s, got %s", "pair", unit.Children[0].UnitType)
  }
    if !(unit.Children[0].Children[0].UnitType == "literal") {
      t.Errorf("Fail unit.Children[0].Children[0].UnitType -- Expected %s, got %s", "literal", unit.Children[0].Children[0].UnitType)
    }
    if !(unit.Children[0].Children[0].Value == 1) {
      t.Errorf("Fail unit.Children[0].Children[0].Value -- Expected %d, got %d", 1, unit.Children[0].Children[0].Value)
    }
    if !(unit.Children[0].Children[1].UnitType == "literal") {
      t.Errorf("Fail unit.Children[0].Children[1].UnitType -- Expected %s, got %s", "literal", unit.Children[0].Children[1].UnitType)
    }
    if !(unit.Children[0].Children[1].Value == 1) {
      t.Errorf("Fail unit.Children[0].Children[1].Value -- Expected %d, got %d", 1, unit.Children[0].Children[1].Value)
    }

  if !(unit.Children[1].UnitType == "pair") {
    t.Errorf("Fail unit.Children[1].UnitType -- Expected %s, got %s", "pair", unit.Children[1].UnitType)
  }
    if !(unit.Children[1].Children[0].UnitType == "literal") {
      t.Errorf("Fail unit.Children[1].Children[0].UnitType -- Expected %s, got %s", "literal", unit.Children[1].Children[0].UnitType)
    }
    if !(unit.Children[1].Children[0].Value == 2) {
      t.Errorf("Fail unit.Children[1].Children[0].Value -- Expected %d, got %d", 2, unit.Children[1].Children[0].Value)
    }
    if !(unit.Children[1].Children[1].UnitType == "literal") {
      t.Errorf("Fail unit.Children[1].Children[1].UnitType -- Expected %s, got %s", "literal", unit.Children[1].Children[1].UnitType)
    }
    if !(unit.Children[1].Children[1].Value == 2) {
      t.Errorf("Fail unit.Children[1].Children[1].Value -- Expected %d, got %d", 2, unit.Children[1].Children[1].Value)
    }
}

func TestSum(t *testing.T) {
  n1 := "[1,1]"
  n2 := "[2,2]"
  expected := "[[1,1],[2,2]]"
  got := sum(n1, n2)

  check(t, expected, got)
}

func TestIsDigit(t *testing.T) {
  if !isDigit("0") {
    t.Errorf("Fail isDigit(\"4\") -- Expected true, got false")
  }
  if !isDigit("4") {
    t.Errorf("Fail isDigit(\"4\") -- Expected true, got false")
  }

  if isDigit("10") {
    t.Errorf("Fail isDigit(\"4\") -- Expected false, got true")
  }
  if isDigit("a") {
    t.Errorf("Fail isDigit(\"a\") -- Expected false, got true")
  }
}

// # Reduction
// ## Explode
func TestReduceRight(t *testing.T) {
  input := "[[[[[9,8],1],2],3],4]"
  expected := "[[[[0,9],2],3],4]"
  got := reduce(input)

  check(t, expected, got)
}

func TestReduceLeft(t *testing.T) {
  input := "[7,[6,[5,[4,[3,2]]]]]"
  expected := "[7,[6,[5,[7,0]]]]"
  got := reduce(input)

  check(t, expected, got)
}

func TestReduceBoth(t *testing.T) {
  input := "[[6,[5,[4,[3,2]]]],1]"
  expected := "[[6,[5,[7,0]]],3]"
  got := reduce(input)

  check(t, expected, got)
}

func TestReduceTwice(t *testing.T) {
  input := "[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]"
  expected := "[[3,[2,[8,0]]],[9,[5,[7,0]]]]"
  got := reduce(input)

  check(t, expected, got)
}

// ## Explode
// Split
// 10: "[5,5]"
// 11: "[5,6]"

// Explodes & Splits
// - [[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]
// - [[[[0,7],4],[7,[[8,4],9]]],[1,1]]
// - [[[[0,7],4],[15,[0,13]]],[1,1]]
// - [[[[0,7],4],[[7,8],[0,13]]],[1,1]]
// - [[[[0,7],4],[[7,8],[0,[6,7]]]],[1,1]]
// - [[[[0,7],4],[[7,8],[6,0]]],[8,1]]

func check(t *testing.T, expected string, got string) {
  if (expected != got) {
    t.Errorf("Fail! Expected %s, got %s", expected, got)
  }
}
