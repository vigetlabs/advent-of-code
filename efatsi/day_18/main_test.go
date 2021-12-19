package main

import (
  // "fmt"
  "testing"
)

// func TestParse1(t *testing.T) {
//   input := "[1,1]"
//   unit := parse(input)
//
//   if !(unit.children[0].unitType == "literal") {
//     t.Errorf("Fail unit.children[0].unitType -- Expected %s, got %s", "literal", unit.children[0].unitType)
//   }
//   if !(unit.children[0].value == 1) {
//     t.Errorf("Fail unit.children[0].value -- Expected %d, got %d", 1, unit.children[0].value)
//   }
//   if !(unit.children[1].unitType == "literal") {
//     t.Errorf("Fail unit.children[1].unitType -- Expected %s, got %s", "literal", unit.children[1].unitType)
//   }
//   if !(unit.children[1].value == 1) {
//     t.Errorf("Fail unit.children[1].value -- Expected %d, got %d", 1, unit.children[1].value)
//   }
// }
//
// func TestParse2(t *testing.T) {
//   input := "[2,2]"
//   unit := parse(input)
//
//   if !(unit.children[0].unitType == "literal") {
//     t.Errorf("Fail unit.children[0].unitType -- Expected %s, got %s", "literal", unit.children[0].unitType)
//   }
//   if !(unit.children[0].value == 2) {
//     t.Errorf("Fail unit.children[0].value -- Expected %d, got %d", 2, unit.children[0].value)
//   }
//   if !(unit.children[1].unitType == "literal") {
//     t.Errorf("Fail unit.children[1].unitType -- Expected %s, got %s", "literal", unit.children[1].unitType)
//   }
//   if !(unit.children[1].value == 2) {
//     t.Errorf("Fail unit.children[1].value -- Expected %d, got %d", 2, unit.children[1].value)
//   }
// }
//
// func TestParse20(t *testing.T) {
//   input := "[20,20]"
//   unit := parse(input)
//
//   if !(unit.children[0].unitType == "literal") {
//     t.Errorf("Fail unit.children[0].unitType -- Expected %s, got %s", "literal", unit.children[0].unitType)
//   }
//   if !(unit.children[0].value == 20) {
//     t.Errorf("Fail unit.children[0].value -- Expected %d, got %d", 20, unit.children[0].value)
//   }
//   if !(unit.children[1].unitType == "literal") {
//     t.Errorf("Fail unit.children[1].unitType -- Expected %s, got %s", "literal", unit.children[1].unitType)
//   }
//   if !(unit.children[1].value == 20) {
//     t.Errorf("Fail unit.children[1].value -- Expected %d, got %d", 20, unit.children[1].value)
//   }
// }

func TestParseNestFirst(t *testing.T) {
  input := "[[1,1],2]"
  unit := parse(input)

  if !(unit.children[0].unitType == "pair") {
    t.Errorf("Fail unit.children[0].unitType -- Expected %s, got %s", "pair", unit.children[0].unitType)
  }
    if !(unit.children[0].children[0].unitType == "literal") {
      t.Errorf("Fail unit.children[0].children[0].unitType -- Expected %s, got %s", "literal", unit.children[0].children[0].unitType)
    }
    if !(unit.children[0].children[0].value == 1) {
      t.Errorf("Fail unit.children[0].children[0].value -- Expected %d, got %d", 1, unit.children[0].children[0].value)
    }
    if !(unit.children[0].children[1].unitType == "literal") {
      t.Errorf("Fail unit.children[0].children[1].unitType -- Expected %s, got %s", "literal", unit.children[0].children[1].unitType)
    }
    if !(unit.children[0].children[1].value == 1) {
      t.Errorf("Fail unit.children[0].children[1].value -- Expected %d, got %d", 1, unit.children[0].children[1].value)
    }

    if !(unit.children[1].unitType == "literal") {
      t.Errorf("Fail unit.children[1].unitType -- Expected %s, got %s", "literal", unit.children[1].unitType)
    }
    if !(unit.children[1].value == 2) {
      t.Errorf("Fail unit.children[1].value -- Expected %d, got %d", 2, unit.children[1].value)
    }

}

func TestParseNestSecond(t *testing.T) {
  input := "[1,[2,2]]"
  unit := parse(input)

  if !(unit.children[0].unitType == "literal") {
    t.Errorf("Fail unit.children[0].unitType -- Expected %s, got %s", "literal", unit.children[0].unitType)
  }
  if !(unit.children[0].value == 1) {
    t.Errorf("Fail unit.children[0].value -- Expected %d, got %d", 1, unit.children[0].value)
  }

  if !(unit.children[1].unitType == "pair") {
    t.Errorf("Fail unit.children[1].unitType -- Expected %s, got %s", "pair", unit.children[1].unitType)
  }
    if !(unit.children[1].children[0].unitType == "literal") {
      t.Errorf("Fail unit.children[1].children[0].unitType -- Expected %s, got %s", "literal", unit.children[1].children[0].unitType)
    }
    if !(unit.children[1].children[0].value == 2) {
      t.Errorf("Fail unit.children[1].children[0].value -- Expected %d, got %d", 2, unit.children[1].children[0].value)
    }
    if !(unit.children[1].children[1].unitType == "literal") {
      t.Errorf("Fail unit.children[1].children[1].unitType -- Expected %s, got %s", "literal", unit.children[1].children[1].unitType)
    }
    if !(unit.children[1].children[1].value == 2) {
      t.Errorf("Fail unit.children[1].children[1].value -- Expected %d, got %d", 2, unit.children[1].children[1].value)
    }
}

func TestParseNestDouble(t *testing.T) {
  input := "[[1,1],[2,2]]"
  unit := parse(input)

  if !(unit.children[0].unitType == "pair") {
    t.Errorf("Fail unit.children[0].unitType -- Expected %s, got %s", "pair", unit.children[0].unitType)
  }
    if !(unit.children[0].children[0].unitType == "literal") {
      t.Errorf("Fail unit.children[0].children[0].unitType -- Expected %s, got %s", "literal", unit.children[0].children[0].unitType)
    }
    if !(unit.children[0].children[0].value == 1) {
      t.Errorf("Fail unit.children[0].children[0].value -- Expected %d, got %d", 1, unit.children[0].children[0].value)
    }
    if !(unit.children[0].children[1].unitType == "literal") {
      t.Errorf("Fail unit.children[0].children[1].unitType -- Expected %s, got %s", "literal", unit.children[0].children[1].unitType)
    }
    if !(unit.children[0].children[1].value == 1) {
      t.Errorf("Fail unit.children[0].children[1].value -- Expected %d, got %d", 1, unit.children[0].children[1].value)
    }

  if !(unit.children[1].unitType == "pair") {
    t.Errorf("Fail unit.children[1].unitType -- Expected %s, got %s", "pair", unit.children[1].unitType)
  }
    if !(unit.children[1].children[0].unitType == "literal") {
      t.Errorf("Fail unit.children[1].children[0].unitType -- Expected %s, got %s", "literal", unit.children[1].children[0].unitType)
    }
    if !(unit.children[1].children[0].value == 2) {
      t.Errorf("Fail unit.children[1].children[0].value -- Expected %d, got %d", 2, unit.children[1].children[0].value)
    }
    if !(unit.children[1].children[1].unitType == "literal") {
      t.Errorf("Fail unit.children[1].children[1].unitType -- Expected %s, got %s", "literal", unit.children[1].children[1].unitType)
    }
    if !(unit.children[1].children[1].value == 2) {
      t.Errorf("Fail unit.children[1].children[1].value -- Expected %d, got %d", 2, unit.children[1].children[1].value)
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

// Explode
// "[[[[[9,8],1],2],3],4]": "[[[[0,9],2],3],4]"
// "[7,[6,[5,[4,[3,2]]]]]": "[7,[6,[5,[7,0]]]]"
// "[[6,[5,[4,[3,2]]]],1]": "[[6,[5,[7,0]]],3]"
// "[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]":
//   first step:  "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]"
//   second step: "[[3,[2,[8,0]]],[9,[5,[7,0]]]]"

// func TestReduce(t *testing.T) {
//   input := "[[[[[9,8],1],2],3],4]"
//   expected := "[[[[0,9],2],3],4]"
//   got := reduce(input)
//
//   check(t, expected, got)
// }

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
