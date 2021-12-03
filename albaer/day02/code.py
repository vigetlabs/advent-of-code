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

# [depth, horizontal_position]

position = [0,0]

def handle_change(change, depth=0, horizontal_position=0):
    direction, distance = change.split()
    pprint("direction: " + direction)
    pprint("distance: " + distance)
    match direction:
        case "down":
            return [depth + int(distance), horizontal_position]
        case "up":
            return[depth - int(distance), horizontal_position]
        case "forward":
            return [depth, horizontal_position + int(distance)]


for change in input_lines:
  position = handle_change(change, position[0], position[1])
  pprint(position)

solution = position[0] * position[1]

write_solution(solution)

# TODO: Refactor things used every day into shareable code
# (reading input anf writing the solution)
