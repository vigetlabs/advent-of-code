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

# 5x5 Indices     Coordinates
# 00,01,02,03,04  00,01,02,03,04
# 05,06,07,08,09  10,11,12,13,14
# 10,11,12,13,14  20,21,22,23,24
# 15,16,17,18,19  30,31,32,33,34
# 20,21,22,23,24  40,41,42,43,44

# 3x4 Indices  Coordinates
# 00,01,02,03  00,01,02,03
# 04,05,06,07  10,11,12,13
# 08,09,10,11  20,21,22,23

# 10x10 Indices/Coordinates
# 00,01,02,03,04,05,06,07,08,09
# 10,11,12,13,14,15,16,17,18,19
# 20,21,22,23,24,25,26,27,28,29
# 30,31,32,33,34,35,36,37,38,39
# ...
# 80,81,82,83,84,85,86,87,88,89
# 90,91,92,93,94,95,96,97,98,99

def flatten(lst):
    return [item for sublist in lst for item in sublist]

def slices(lst, n):
    return[lst[i:i + n] for i in range(0, len(lst), n)]

def get_int_lst(lst):
    string_chars = flatten([list(row) for row in lst])
    return [int(char) for char in string_chars]

def get_string_lst(int_lst, width):
    string_flat_list = [str(i) for i in int_lst]
    string_sublists = slices(string_flat_list, width)
    return ["".join(i) for i in string_sublists]

def printable_char(char):
    match char:
        case 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9:
            return str(char)
        case 10:
            return "x"
        case _:
            return "-"

def printable_lst(lst, int_lst):
    width, _ = get_dimensions(lst)
    char_list = [printable_char(i) for i in int_lst]
    char_sublists = slices(char_list, width)
    char_strings = ["".join(i) for i in char_sublists]
    return "\n".join(char_strings) + "\n"

def print_lst(lst, int_lst):
    print(printable_lst(lst, int_lst))

def get_dimensions(lst):
    return [len(lst[0]), len(lst)]

def get_coords(lst, index):
    width, _ = get_dimensions(lst)
    return list(divmod(index, width))

def all_indices(lst):
    width, height = get_dimensions(lst)
    return [i for i in range(width * height)]

def is_adjacent(lst, index_a, index_b):
    ax, ay = get_coords(lst, index_a)
    bx, by = get_coords(lst, index_b)
    return (abs(ax - bx) <= 1) and (abs(ay - by) <= 1)

def adjacent_indexes(lst, index):
    return [i for i in all_indices(lst) if is_adjacent(lst, i, index)]

def adjacent_increases(lst, int_lst, index):
    indexes = adjacent_indexes(lst, index)
    return [i for i in indexes if int_lst[i] != 10 or i == index]


def add_one_to_all(int_lst):
    return [i + 1 for i in int_lst]

def flash_one(lst, int_lst, index):
    width, _ = get_dimensions(lst)
    increases = adjacent_increases(lst, int_lst, index)
    new_int_lst = [val + 1 if (idx in increases) else val for idx, val in enumerate(int_lst)]
    print_lst(lst, int_lst)
    return new_int_lst

def flash_grid(lst, int_lst):
    width, _ = get_dimensions(lst)
    if not 10 in int_lst:
        return int_lst
    else:
        next_flash_index = int_lst.index(10)
        new_int_lst = flash_one(lst, int_lst, next_flash_index)
        return flash_grid(lst, new_int_lst)

def reset_grid(int_lst):
    return [0 if i > 9 else i for i in int_lst]

def step(lst):
    width, _ = get_dimensions(lst)
    int_lst = get_int_lst(lst)
    int_lst = add_one_to_all(int_lst)
    int_lst = flash_grid(lst, int_lst)
    int_lst = reset_grid(int_lst)
    lst = get_string_lst(int_lst, width)
    return lst

def count_flashes_for_steps(lst, step_count, flash_count):
    if step_count == 0:
        return flash_count
    else:
        new_grid = step(lst)
        new_flash_count = flash_count + new_grid.count(0)
        return steps(new_grid, step_count - 1, new_flash_count)







# def adjacent(lst, index):
#     width, height = get_dimensions(lst)


#     above = index - width,
#     target = index
#     below = index + height

#     return [i for i in all_ints]

# def string_grid_to_int_grid(string_grid):
#     return [[int(i) for i in list(string_row)] for string_row in string_grid]

# def int_grid_to_string_grid(int_grid):
#     return ["".join(str(j) for j in i) for i in int_grid]

# def printable_char(char):
#     match char:
#         case 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9:
#             return str(char)
#         case 10:
#             return "x"
#         case _:
#             return "-"

# def printable_grid(int_grid):
#     return "\n".join(["".join(printable_char(j) for j in i) for i in int_grid]) + "\n"

# def add_one_to_grid(int_grid):
#     return [[i + 1 for i in row] for row in int_grid]

# def in_grid(int_grid, coord):
#     max_x = len(int_grid[0])
#     max_y = len(int_grid)
#     return coord[0] < max_x and coord[1] < max_y

# def adjacent_coords(grid, x, y):
#     coords = [
#         [x - 1, y - 1],
#         [x - 1, y],
#         [x - 1, y + 1],
#         [x, y - 1],
#         [x, y],
#         [x, y + 1],
#         [x + 1, y - 1],
#         [x + 1, y],
#         [x + 1, y + 1],
#     ]
#     return [coord for coord in coords if in_grid(grid, coord)]

# def flash_coord(grid, x, y):
#     adjacents = adjacent_coords(grid, x, y)
#     for coord in adjacents:
#         x, y = coord
#         grid[x][y] = int(grid[x][y]) + 1
#     return grid

# def flash_row(grid, x):
#     for y, i in enumerate(grid[x]):
#         if i > 9:
#             grid = flash_coord(grid, x, y)
#     return grid

# def flash_grid(grid, x=0):
#     print(printable_grid(grid))
#     if x == len(grid):
#         return grid
#     else:
#         new_grid = flash_row(grid, x)
#         return flash_grid(new_grid, x + 1)

# def reset_row(row):
#     return [0 if int(i) > 9 else int(i) for i in row]

# def reset_grid(grid):
#     return [reset_row(row) for row in grid]

# def step(int_grid):
#     int_grid = add_one_to_grid(int_grid)
#     int_grid = flash_grid(int_grid)
#     int_grid = reset_grid(int_grid)
#     return int_grid_to_string_grid(int_grid)

# def count_zeroes(string_grid):
#   return sum([i.count('0') for i in string_grid])

# def go(grid, step_count=0, flash_count=0):
#     if step_count == 0:
#         return [grid, flash_count]
#     else:
#         new_grid = step(grid)
#         new_flash_count = flash_count + count_zeroes(new_grid)
#         go(new_grid, step_count - 1, new_flash_count)

# Write solution

if __name__ == '__main__':
    part_1_result = "TODO"
    part_2_result = "TODO"
    solution = str(part_1_result) + "\n" + str(part_2_result)
    write_solution(solution)
