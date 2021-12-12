from pprint import pprint

def write_solution(solution, output_file_path="solution.txt"):
    with open(output_file_path, "w") as output_file:
        output_file.write(str(solution))
        print(solution)
        return solution

def read_input_lines(input_file_path="input.txt"):
    with open(input_file_path, "r") as input_file:
        return input_file.read().splitlines()

def split_inputs_from_first_line():
    input_lines = read_input_lines()
    first_line_as_str = input_lines[0]
    return first_line_as_str.split(",")

# Part 1

# +1 to all
# all >9s flash
# Octopuses adjacent to flashes +1
# All >9s flash
# Octopuses adjacent to flashes +1
# ...
# Reset all >9s to 0

def string_grid_to_int_grid(string_grid):
    return [[int(i) for i in list(string_row)] for string_row in string_grid]

def int_grid_to_string_grid(int_grid):
    return ["".join(str(j) for j in i) for i in int_grid]

def printable_char(char):
    match char:
        case 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9:
            return str(char)
        case 10:
            return "x"
        case _:
            return "-"

def printable_grid(int_grid):
    return "\n".join(["".join(printable_char(j) for j in i) for i in int_grid]) + "\n"

def add_one_to_grid(int_grid):
    return [[i + 1 for i in row] for row in int_grid]

def in_grid(int_grid, coord):
    max_x = len(int_grid[0])
    max_y = len(int_grid)
    return coord[0] < max_x and coord[1] < max_y

def adjacent_coords(grid, x, y):
    coords = [
        [x - 1, y - 1],
        [x - 1, y],
        [x - 1, y + 1],
        [x, y - 1],
        [x, y],
        [x, y + 1],
        [x + 1, y - 1],
        [x + 1, y],
        [x + 1, y + 1],
    ]
    return [coord for coord in coords if in_grid(grid, coord)]

def flash_coord(grid, x, y):
    adjacents = adjacent_coords(grid, x, y)
    for coord in adjacents:
        x, y = coord
        grid[x][y] = int(grid[x][y]) + 1
    return grid

def flash_row(grid, x):
    for y, i in enumerate(grid[x]):
        if i > 9:
            grid = flash_coord(grid, x, y)
    return grid

def flash_grid(grid, x=0):
    print(printable_grid(grid))
    if x == len(grid):
        return grid
    else:
        new_grid = flash_row(grid, x)
        return flash_grid(new_grid, x + 1)

def reset_row(row):
    return [0 if int(i) > 9 else int(i) for i in row]

def reset_grid(grid):
    return [reset_row(row) for row in grid]

def step(int_grid):
    int_grid = add_one_to_grid(int_grid)
    int_grid = flash_grid(int_grid)
    int_grid = reset_grid(int_grid)
    return int_grid_to_string_grid(int_grid)

def count_zeroes(string_grid):
  return sum([i.count('0') for i in string_grid])

def go(grid, step_count=0, flash_count=0):
    if step_count == 0:
        return [grid, flash_count]
    else:
        new_grid = step(grid)
        new_flash_count = flash_count + count_zeroes(new_grid)
        go(new_grid, step_count - 1, new_flash_count)

# Write solution

if __name__ == '__main__':
    part_1_result = "TODO"
    part_2_result = "TODO"
    solution = str(part_1_result) + "\n" + str(part_2_result)
    write_solution(solution)
