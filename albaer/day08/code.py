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

def id_1(digits_lst):
    pattern_1 = [i for i in digits_lst if len(i) == 2][0]
    pprint("1 is " + pattern_1)
    return pattern_1

def id_4(digits_lst):
    pattern_4 = [i for i in digits_lst if len(i) == 4][0]
    pprint("4 is " + pattern_4)
    return pattern_4

def id_7(digits_lst):
    pattern_7 = [i for i in digits_lst if len(i) == 3][0]
    pprint("7 is " + pattern_7)
    return pattern_7

def id_8(digits_lst):
    pattern_8 = [i for i in digits_lst if len(i) == 7][0]
    pprint("8 is " + pattern_8)
    return pattern_8

def id_pattern(digits_lst, number):
    match number:
        case 1:
            return id_1(digits_lst)
        case 4:
            return id_4(digits_lst)
        case 7:
            return id_7(digits_lst)
        case 8:
            return id_8(digits_lst)
        case _:
            pprint(f'cannot id pattern for {number}.')
            return "UNKNOWN"

def read_output(entry):
    sps, ovs = split_entry(entry)
    for i in range(0,10):
        pattern = id_pattern(sps, i)
        ovs = [pattern if i == pattern else i for i in ovs]
    result = " ".join([str(i) for i in ovs])
    pprint(result)
    return result


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
