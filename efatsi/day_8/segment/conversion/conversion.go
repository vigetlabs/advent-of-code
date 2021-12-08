package conversion

import (
  "fmt"
)

func GenerateConversion(calibratingDigits [10]string) map[string]string {
  fmt.Println("calibratingDigits: ", calibratingDigits)

  conversion := make(map[string]string)
  conversion["a"] = "f"
  return conversion
}
