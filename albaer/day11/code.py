from pprint import pprint
from math import sqrt

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

def flatten(lst):
    return [item for sublist in lst for item in sublist]

def slices(lst, n):
    return[lst[i:i + n] for i in range(0, len(lst), n)]

def get_int_lst(lst):
    string_chars = flatten([list(row) for row in lst])
    return [int(char) for char in string_chars]

def get_string_lst(int_lst):
    width = int(sqrt(len(int_lst)))
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
    width = int(sqrt(len(int_lst)))
    char_list = [printable_char(i) for i in int_lst]
    char_sublists = slices(char_list, width)
    char_strings = ["".join(i) for i in char_sublists]
    return "\n".join(char_strings) + "\n"

def print_lst(lst, int_lst):
    print(printable_lst(lst, int_lst))

def get_dimensions(lst):
    return [len(lst[0]), len(lst)]

def get_coords(int_lst, index):
    width = int(sqrt(len(int_lst)))
    return list(divmod(index, width))

def is_adjacent(int_lst, index_a, index_b):
    ax, ay = get_coords(int_lst, index_a)
    bx, by = get_coords(int_lst, index_b)
    return (abs(ax - bx) <= 1) and (abs(ay - by) <= 1)

def adjacent_indexes(int_lst, index):
    return [i for i in range(len(int_lst)) if is_adjacent(int_lst, i, index)]

def adjacent_increases(int_lst, index):
    indexes = adjacent_indexes(int_lst, index)
    return [i for i in indexes if int_lst[i] != 10 or i == index]

def add_one_to_all(int_lst):
    return [i + 1 for i in int_lst]

def flash_one(int_lst, index):
    width = int(sqrt(len(int_lst)))
    increases = adjacent_increases(int_lst, index)
    new_int_lst = [val + 1 if (idx in increases) else val for idx, val in enumerate(int_lst)]
    return new_int_lst

def flash_grid(int_lst):
    width = int(sqrt(len(int_lst)))
    if not 10 in int_lst:
        return int_lst
    else:
        next_flash_index = int_lst.index(10)
        new_int_lst = flash_one(int_lst, next_flash_index)
        return flash_grid(new_int_lst)

def reset_grid(int_lst):
    return [0 if i > 9 else i for i in int_lst]

def step(lst):
    int_lst = get_int_lst(lst)
    int_lst = add_one_to_all(int_lst)
    int_lst = flash_grid(int_lst)
    int_lst = reset_grid(int_lst)
    lst = get_string_lst(int_lst)
    return lst

def count_zeroes(lst):
    int_lst = get_int_lst(lst)
    return int_lst.count(0)

def count_flashes(lst, step_count, flash_count=0):
    if step_count == 0:
        return flash_count
    else:
        new_lst = step(lst)
        new_flash_count = flash_count + count_zeroes(new_lst)
        return count_flashes(new_lst, step_count - 1, new_flash_count)

# Part 2
def find_simultaneous_flashes(lst, step_count=0):
    width, height = get_dimensions(lst)
    if count_zeroes(lst) == width * height:
        return step_count
    else:
        new_lst = step(lst)
        return find_simultaneous_flashes(new_lst, step_count + 1)

# Write solution

if __name__ == '__main__':
    inputs = read_input_lines()
    part_1_result = count_flashes(inputs, 100)
    part_2_result = find_simultaneous_flashes(inputs)
    solution = str(part_1_result) + "\n" + str(part_2_result)
    write_solution(solution)
