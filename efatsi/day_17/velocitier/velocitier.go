package velocitier

type Velocitier struct {
  infiniteXs map[int]int
}

func New(infiniteXs map[int]int) Velocitier {
  return Velocitier{infiniteXs}
}

func (v *Velocitier) GetYVelocity(yTarget int, step int) int {
  return step + getOffset(yTarget, step)
}

func (v *Velocitier) GetXVelocity(xTarget int, step int) int {
  for velocity, target := range v.infiniteXs {
    if (step >= velocity && target == xTarget) { return velocity }
  }

  return step + getOffset(xTarget, step)
}

func getOffset(target int, step int) int {
  // eg: if heading towards -9 and it takes 2 steps
  // - steps would be [-4, -5]
  // - offset is calculatable by starting with sum([2, 1]) (`addToZero(step)`) and
  //   figuring out how much each individual element needs to adjest to add up to `target`
  // - in this case: (-9 - 3) / 2 = -6
  //
  // => so the first step of a downward trending array w/ len == steps and sum == target:
  //    2 + -6 == -4
  return (target - addToZero(step)) / step
}

// -- helpers --

func addToZero(input int) int {
  // input + input-1 + input-2 + ... + 0
  return input * (input + 1) / 2
}
