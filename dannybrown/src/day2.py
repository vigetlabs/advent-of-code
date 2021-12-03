from helpers import read_string_puzzle_input

HORIZONTAL_DIRECTIONS = ['FORWARD']
VERTICAL_DIRECTIONS = ['UP', 'DOWN']
def day2_part1(instructions):
    horizontal_pos = 0
    vertical_pos = 0

    for instruction in instructions:
      direction, distance = instruction.split()

      if direction.upper() in HORIZONTAL_DIRECTIONS:
        horizontal_pos += int(distance)
      elif direction.upper() in VERTICAL_DIRECTIONS:
        if direction == 'up':
          vertical_pos -= int(distance)
        else:
          vertical_pos += int(distance)

    return horizontal_pos,  vertical_pos

def day2_part2(instructions):
    horizontal_pos = 0
    vertical_pos = 0
    aim = 0
    for instruction in instructions:
      direction, distance = instruction.split()

      if direction.upper() in HORIZONTAL_DIRECTIONS:
        horizontal_pos += int(distance)
        vertical_pos += aim * int(distance)
      elif direction.upper() in VERTICAL_DIRECTIONS:
        if direction == 'up':
          aim -= int(distance)
        else:
          aim += int(distance)

    return horizontal_pos, vertical_pos

if __name__ == '__main__':
  instructions = read_string_puzzle_input("dannybrown/input/day2.txt")

  horiz_1, vert_1 = day2_part1(instructions)
  horiz_2, vert_2 = day2_part2(instructions)

  print("Day 1 Part 1:", horiz_1 * vert_1)
  print("Day 2 Part 2:", horiz_2 * vert_2)


