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

COMPLETION_POINTS = {
    ")": 1,
    "]": 2,
    "}": 3,
    ">": 4,
}

def calculate_syntax_points(line):
    if line == "":
        return 0

    pattern = r"([\{\[\(\<])([\}\]\)\>])"
    m = re.search(pattern, line)

    if m:
        pair = m.group(0)
        open_char = m.group(1)
        close_char = m.group(2)
        valid_close_char = VALID_PAIRS[open_char]

        if close_char == valid_close_char:
            return calculate_syntax_points(line.replace(pair, ""))
        else:
            return POINTS[close_char]
    else:
        return 0

def total_syntax_points(lines):
    return sum([calculate_syntax_points(i) for i in lines])
# Part 2

def find_completion_string(line):
    if line == "":
        return ""

    pattern = r"([\{\[\(\<])([\}\]\)\>])"
    m = re.search(pattern, line)

    if m:
        pair = m.group(0)
        open_char = m.group(1)
        close_char = m.group(2)
        valid_close_char = VALID_PAIRS[open_char]

        if close_char == valid_close_char:
            return find_completion_string(line.replace(pair, ""))
        else:
            return ""
    else:
        open_chars = list(line)
        close_chars = [VALID_PAIRS[i] for i in open_chars][::-1]
        completion_string = "".join(close_chars)
        return completion_string

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
    middle_index = int((len(sorted_scores) - 1)/2)
    return sorted_scores[middle_index]

# Write solution

if __name__ == '__main__':
    inputs = read_input_lines()
    part_1_result = total_syntax_points(inputs)
    part_2_result = score_completion_lines(inputs)
    solution = str(part_1_result) + "\n" + str(part_2_result)
    write_solution(solution)
