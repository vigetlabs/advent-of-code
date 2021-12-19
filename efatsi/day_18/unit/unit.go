package unit

import (
  "fmt"
  "strconv"
)

type Unit struct {
  Depth int
  Parent *Unit
  UnitType string // "pair" | "literal"
  Children [](*Unit)
  Value int
}

// -- Counstructors --
func NewBase() *Unit  {
  return &Unit {
    UnitType: "pair",
    Depth: 0,
  }
}

func (u *Unit) AddPair() *Unit {
  child := &Unit {
    UnitType: "pair",
    Depth: u.Depth + 1,
    Parent: u,
  }
  u.Children = append(u.Children, child)

  return child
}

func (u *Unit) AddLiteral(Value int) *Unit {
  child := &Unit {
    UnitType: "literal",
    Depth: u.Depth + 1,
    Parent: u,
    Value: Value,
  }
  u.Children = append(u.Children, child)

  return child
}

// -- Instance Methods --

func (u *Unit) Reduce() *Unit {
  // loop thru unit.Children
  // - if any should explode
  //   - explodeChild
  //   - reduceUnit(unit)
  // - if any should split
  //   - splitChild
  //   - reduceUnit(unit)

  for _, child := range u.Children {
    if child.checkExplosion() {
      return u.Reduce()
    }
    // if child.shouldSplit() {
    //   child.split()
    //   return reduceUnit(unit)
    // }
  }

  return u
}

func (u *Unit) checkExplosion() bool {
  if u.UnitType != "pair" { return false }

  if u.Depth >= 4 {
    u.explode()
    return true
  } else {
    for _, child := range u.Children {
      if child.checkExplosion() {
        return true
      }
    }
  }

  return false
}

func (u *Unit) explode() {
  orderedUnits := u.topParent().assembleOrderedUnits()

  var explodingIndex int
  for i, unit := range orderedUnits {
    if u == unit { explodingIndex = i }
  }

  // Scan for left digit home
  foundLeft := false
  for i := explodingIndex - 1; i > 0 && !foundLeft; i-- {
    if orderedUnits[i].UnitType == "literal" {
      orderedUnits[i].Value += u.Children[0].Value
      foundLeft = true
    }
  }

  // Scan for right digit home
  // Start at explodingIndex + 3 to skip the literals within the exploding pair
  foundRight := false
  for i := explodingIndex + 3; i < len(orderedUnits) && !foundRight; i++ {
    if orderedUnits[i].UnitType == "literal" {
      orderedUnits[i].Value += u.Children[1].Value
      foundRight = true
    }
  }

  u.UnitType = "literal"
  u.Children = []*Unit{}
  u.Value = 0
}

func (u *Unit) topParent() *Unit {
  if u.Parent == nil {
    return u
  } else {
    return u.Parent.topParent()
  }
}

func (u *Unit) assembleOrderedUnits() []*Unit {
  units := []*Unit{u}

  if u.UnitType == "pair" {
    units = append(units, u.Children[0].assembleOrderedUnits()...)
    units = append(units, u.Children[1].assembleOrderedUnits()...)
  }

  return units
}

func (u *Unit) ToString() string {
  if u.UnitType == "literal" {
    return strconv.Itoa(u.Value)
  } else {
    return "[" + u.Children[0].ToString() + "," + u.Children[1].ToString() + "]"
  }
}

// -- helpers --

func (u *Unit) Print() {
  spacing := ""
  for i := 0; i < u.Depth; i++ {
    spacing += "  "
  }

  fmt.Println(spacing, "- Unit Type:  ", u.UnitType)
  fmt.Println(spacing, "- Depth:      ", u.Depth)
  if (u.UnitType == "literal") {
    fmt.Println(spacing, "- Value:      ", u.Value)
  } else {
    fmt.Println(spacing, "- # Children: ", len(u.Children))
  }

  fmt.Println("")

  for _, child := range u.Children {
    child.Print()
  }
}

func (u *Unit) PrintSelf() {
  spacing := ""
  for i := 0; i < u.Depth; i++ {
    spacing += "  "
  }

  fmt.Println(spacing, "- Unit Type:  ", u.UnitType)
  fmt.Println(spacing, "- Depth:      ", u.Depth)
  if (u.UnitType == "literal") {
    fmt.Println(spacing, "- Value:      ", u.Value)
  } else {
    fmt.Println(spacing, "- # Children: ", len(u.Children))
  }

  fmt.Println("")
}
