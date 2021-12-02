from helpers import read_puzzle_input

def day1_part1(depths):
    length = len(depths)
    sum = 0
    for i in range(length):
      if i == length - 1:
        break
      sum += depths[i] < depths[i+1]

    return sum

def day1_part2(depths):
  length = len(depths)
  sum = 0

  for i in range(len(depths)):
    if i + 2  == length - 1:
      break

    first_batch = depths[i] + depths[i+1] + depths[i+2]
    second_batch = depths[i+1] + depths[i+2] + depths[i+3]

    if first_batch < second_batch:
      sum += 1
  return sum

if __name__ == '__main__':
  depths = read_puzzle_input("dannybrown/input/day1.txt")

  print("Day 1 Part 1:", day1_part1(depths))
  print("Day 2 Part 2:", day1_part2(depths))


