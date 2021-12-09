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

def split_entry(entry):
    sp_str, ov_str = entry.split(" | ")
    return [sp_str.split(" "), ov_str.split(" ")]

def get_entries(entries_lst):
    return [split_entry(entry) for entry in entries_lst]

def get_output_values(entries_lst):
    entries = get_entries(entries_lst)
    return [i[1] for i in entries]

def count_1s(digits_lst):
    return sum(len(i) == 2 for i in digits_lst)

def count_4s(digits_lst):
    return sum(len(i) == 4 for i in digits_lst)

def count_7s(digits_lst):
    return sum(len(i) == 3 for i in digits_lst)

def count_8s(digits_lst):
    return sum(len(i) == 7 for i in digits_lst)

def count_1_4_7_8_digits(digits_lst):
    return sum([count_1s(digits_lst), count_4s(digits_lst), count_7s(digits_lst), count_8s(digits_lst)])

def count_1_4_7_8(entries_lst):
    output_values = get_output_values(entries_lst)
    return sum([count_1_4_7_8_digits(digits_lst) for digits_lst in output_values])

# Part 2

# def id_1(digits_lst):
#     pattern_1 = [i for i in digits_lst if len(i) == 2][0]
#     pprint("1 is " + pattern_1)
#     return pattern_1

# def read_output(entry):
#     pattern 1 = id_1


def sum_output_values(entries_lst):
    # pattern = [id_1(i["sps"]) for i in get_entries(entries_lst)]

    return 0

# Write solution

if __name__ == '__main__':
    entries = read_input_lines()
    part_1_result = count_1_4_7_8(entries)
    part_2_result = "TODO"
    solution = str(part_1_result) + "\n" + str(part_2_result)
    write_solution(solution)
