package value

func Sum(values []int) int {
  sum := 0
  for _, val := range values {
    sum += val
  }
  return sum
}

func Product(values []int) int {
  product := 1
  for _, val := range values {
    product *= val
  }
  return product
}

func Min(values []int) int {
  min := 1000000
  for _, val := range values {
    if (val < min) { min = val }
  }
  return min
}

func Max(values []int) int {
  max := 0
  for _, val := range values {
    if (val > max) { max = val }
  }
  return max
}

func GreaterThan(values []int) int {
  if values[0] > values[1] {
    return 1
  } else {
    return 0
  }
}

func LessThan(values []int) int {
  if values[0] < values[1] {
    return 1
  } else {
    return 0
  }
}

func EqualTo(values []int) int {
  if values[0] == values[1] {
    return 1
  } else {
    return 0
  }
}
