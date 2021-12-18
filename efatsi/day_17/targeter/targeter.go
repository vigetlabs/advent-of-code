package targeter

type Targeter struct {
  minX int
  maxX int
  minY int
  maxY int
  infiniteXs map[int]int
}

func New(minX int, maxX int, minY int, maxY int, infiniteXs map[int]int) Targeter {
  return Targeter{minX, maxX, minY, maxY, infiniteXs}
}

func (t *Targeter) GetYTargets(step int) []int {
  yTargets := make([]int, 0)

  for y := t.minY; y <= t.maxY; y++ {
    if t.validYStep(y, step) {
      yTargets = append(yTargets, y)
    }
  }

  return yTargets
}

func (t *Targeter) GetXTargets(step int) []int {
  xTargets := make([]int, 0)

  for x := t.minX; x <= t.maxX; x++ {
    if t.validXStep(x, step) {
      xTargets = append(xTargets, x)
    }
  }

  return xTargets
}

func (t *Targeter) validYStep(y int, step int) bool {
  // Nothing fancy about y steps except if the modulo matches
  return moduloMatch(y, step)
}

func (t *Targeter) validXStep(x int, step int) bool {
  // x is a little trickier:
  // - check if modulo matches AND it's even possible for x to get there (since x velocity can never be negative)
  // - OR check if we're on an infinite dropping column

  return (moduloMatch(x, step) && x >= addToZero(step)) || contains(t.infiniteColsLessThan(step), x)
}

func (t *Targeter) infiniteColsLessThan(step int) []int {
  infiniteCols := make([]int, 0)

  for xVelocity, _ := range t.infiniteXs {
    if xVelocity < step {
      infiniteCols = append(infiniteCols, addToZero(xVelocity))
    }
  }

  return infiniteCols
}

// -- helpers --

func moduloMatch(val int, step int) bool {
  if step % 2 == 0 {
    // even steps (eg: 5+4+3+2=14 -- 4 steps puts you on positions in between multiples of 4)
    return (val + (step/2)) % step == 0
  } else {
    // odd steps (eg: 11+10+9=30 -- 3 steps puts you on positions that are multiples of 3)
    return val % step == 0
  }
}

func addToZero(input int) int {
  // input + input-1 + input-2 + ... + 0
  return input * (input + 1) / 2
}

func contains(slice []int, value int) bool {
  for _, v := range slice {
    if v == value {
      return true
    }
  }

  return false
}
