from pprint import pprint

input_file_path = "input.txt"
output_file_path = "solution.txt"

def write_solution(solution):
    with open(output_file_path, "w") as output_file:
        output_file.write(str(solution))
        print(solution)
        return solution

def read_input_lines():
    with open(input_file_path, "r") as input_file:
        return input_file.read().splitlines()

input_lines = read_input_lines()

# Part 1

string_changes = [line.split() for line in input_lines]
changes = [[change[0], int(change[1])] for change in string_changes]

position = [0,0]

def handle_change(change, depth=0, horizontal_position=0):
    direction, distance = change
    match direction:
        case "down":
            return [depth + distance, horizontal_position]
        case "up":
            return[depth - distance, horizontal_position]
        case "forward":
            return [depth, horizontal_position + distance]

for change in changes:
  position = handle_change(change, position[0], position[1])
  pprint(position)

part_1_result = position[0] * position[1]

# Part 2

position_and_aim = [0,0,0]

def handle_aim_change(change, depth=0, horizontal_position=0, aim=0):
    direction, distance = change
    match direction:
        case "down":
            return [depth, horizontal_position, aim + distance]
        case "up":
            return [depth, horizontal_position, aim - distance]
        case "forward":
            return [depth + aim * distance, horizontal_position + distance, aim]

for change in changes:
  position_and_aim = handle_aim_change(change, position_and_aim[0], position_and_aim[1], position_and_aim[2])
  pprint(position_and_aim)

part_2_result = position_and_aim[0] * position_and_aim[1]
pprint(part_2_result)

# Write solution

solution = str(part_1_result) + "\n" + str(part_2_result)
write_solution(solution)

# TODO: Clean this up! Find more python stuff.
# TODO: Refactor things used every day into shareable code
# (reading input and writing the solution)
