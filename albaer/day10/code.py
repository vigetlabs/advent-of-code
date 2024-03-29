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

# Constants

VALID_PAIRS = {
  "{": "}",
  "[": "]",
  "<": ">",
  "(": ")",
}

SYNTAX_POINTS = {
    ")": 3,
    "]": 57,
    "}": 1197,
    ">": 25137,
}

COMPLETION_POINTS = {
    ")": 1,
    "]": 2,
    "}": 3,
    ">": 4,
}

PAIR_PATTERN = r"([\{\[\(\<])([\}\]\)\>])"

# Utilities

def middle_element(lst):
    middle_index = int((len(lst) - 1)/2)
    return lst[middle_index]

# Part 1

def check_pair(pair):
    open_char, close_char = list(pair)
    valid_close_char = VALID_PAIRS[open_char]
    return close_char == valid_close_char

def analyze_line(line):
    # All pairs removed => valid line
    if line == "":
        return ["valid", line]

    m = re.search(PAIR_PATTERN, line)
    if m:
        pair = m.group(0)
        pair_is_valid = check_pair(pair)

    # Valid pair => Remove it and find the next one
        if pair_is_valid:
            new_line = line.replace(pair, "")
            return analyze_line(new_line)
        else:
    # Invalid pair => Line is corrupt
            return ["corrupt", line]
    else:
    # Incomplete line
        return ["incomplete", line]

def calculate_syntax_points(line):
    state, remaining = analyze_line(line)

    if state == "corrupt":
        m = re.search(PAIR_PATTERN, remaining)
        pair = m.group(0)
        return SYNTAX_POINTS[pair[1]]
    else:
        return 0

def total_syntax_points(lines):
    return sum([calculate_syntax_points(i) for i in lines])

# Part 2

def completion_string(line):
    open_chars = list(line)
    close_chars = [VALID_PAIRS[i] for i in open_chars][::-1]
    return "".join(close_chars)

def find_completion_string(line):
    state, remaining = analyze_line(line)

    if state == "incomplete":
        return completion_string(remaining)
    else:
        return ""

def score_completion_string(completion_string, score=0):
    if completion_string == "":
        return score
    else:
        new_score = (score * 5) + COMPLETION_POINTS[completion_string[0]]
        new_string = completion_string[1:]
        return score_completion_string(new_string, new_score)

def score_completion_line(line):
    completion_string = find_completion_string(line)
    return score_completion_string(completion_string)

def score_completion_lines(lst):
    completion_scores = [score_completion_line(i) for i in lst]
    sorted_scores = sorted([i for i in completion_scores if i != 0])
    return middle_element(sorted_scores)

# Write solution

if __name__ == '__main__':
    inputs = read_input_lines()
    part_1_result = total_syntax_points(inputs)
    part_2_result = score_completion_lines(inputs)
    solution = str(part_1_result) + "\n" + str(part_2_result)
    write_solution(solution)
