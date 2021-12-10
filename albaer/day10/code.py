from pprint import pprint
import re

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

VALID_PAIRS = {
  "{": "}",
  "[": "]",
  "<": ">",
  "(": ")",
}

POINTS = {
    ")": 3,
    "]": 57,
    "}": 1197,
    ">": 25137,
}

def calculate_points(line):
    pprint(line)
    if line == "":
        pprint("valid")
        return 0

    pattern = r"([\{\[\(\<])([\}\]\)\>])"
    m = re.search(pattern, line)

    if m:
        pair = m.group(0)
        open_char = m.group(1)
        close_char = m.group(2)
        valid_close_char = VALID_PAIRS[open_char]

        if close_char == valid_close_char:
            pprint(f"removing {pair}")
            return calculate_points(line.replace(pair, ""))
        else:
            pprint(f"invalid: expected {valid_close_char} but got {close_char} instead")
            return POINTS[close_char]
    else:
        pprint("incomplete")
        return 0

def total_points(lines):
    return sum([calculate_points(i) for i in lines])

# Write solution

if __name__ == '__main__':
    inputs = read_input_lines()
    part_1_result = total_points(inputs)
    part_2_result = "TODO"
    solution = str(part_1_result) + "\n" + str(part_2_result)
    write_solution(solution)
