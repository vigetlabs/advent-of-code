package translation

type Translation = func(int, int, int) (int, int, int)

// Facing x
func Translate1(x int, y int, z int) (int, int, int) {
  return x, y, z
}
func Translate2(x int, y int, z int) (int, int, int) {
  return x, -y, -z
}
func Translate3(x int, y int, z int) (int, int, int) {
  return x, -z, y
}
func Translate4(x int, y int, z int) (int, int, int) {
  return x, z, -y
}

// Facing -x
func Translate5(x int, y int, z int) (int, int, int) {
  return -x, -y, z
}
func Translate6(x int, y int, z int) (int, int, int) {
  return -x, y, -z
}
func Translate7(x int, y int, z int) (int, int, int) {
  return -x, -z, -y
}
func Translate8(x int, y int, z int) (int, int, int) {
  return -x, z, y
}

// Facing y
func Translate9(x int, y int, z int) (int, int, int) {
  return y, -x, z
}
func Translate10(x int, y int, z int) (int, int, int) {
  return y, x, -z
}
func Translate11(x int, y int, z int) (int, int, int) {
  return y, z, x
}
func Translate12(x int, y int, z int) (int, int, int) {
  return y, -z, -x
}

// Facing -y
func Translate13(x int, y int, z int) (int, int, int) {
  return -y, x, z
}
func Translate14(x int, y int, z int) (int, int, int) {
  return -y, -x, -z
}
func Translate15(x int, y int, z int) (int, int, int) {
  return -y, -z, x
}
func Translate16(x int, y int, z int) (int, int, int) {
  return -y, z, -x
}

// Facing z
func Translate17(x int, y int, z int) (int, int, int) {
  return z, x, y
}
func Translate18(x int, y int, z int) (int, int, int) {
  return z, -x, -y
}
func Translate19(x int, y int, z int) (int, int, int) {
  return z, -y, x
}
func Translate20(x int, y int, z int) (int, int, int) {
  return z, y, -x
}

// Facing -z
func Translate21(x int, y int, z int) (int, int, int) {
  return -z, -x, y
}
func Translate22(x int, y int, z int) (int, int, int) {
  return -z, x, -y
}
func Translate23(x int, y int, z int) (int, int, int) {
  return -z, -y, -x
}
func Translate24(x int, y int, z int) (int, int, int) {
  return -z, y, x
}
