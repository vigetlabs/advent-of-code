def read_puzzle_input():
  ret = []
  with open('input/day1.txt') as f:
    for line in f:
      ret.append(int(line))
  return ret


if __name__ == '__main__':
  depths = read_puzzle_input()
  sum = 0
  for i in range(len(depths)):
    if i == len(depths) - 1:
      break
    sum += depths[i] < depths[i+1]

  print(sum)


