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

def id_0(digits_lst):
    nine_pattern = id_9(digits_lst)
    one_letters = list(id_1(digits_lst))
    return [i for i in digits_lst if (len(i) == 6 and i != nine_pattern and all(j in i for j in one_letters))][0]

def id_1(digits_lst):
    return [i for i in digits_lst if len(i) == 2][0]

def id_4(digits_lst):
    return [i for i in digits_lst if len(i) == 4][0]

def id_6(digits_lst):
    nine_pattern = id_9(digits_lst)
    zero_pattern = id_0(digits_lst)
    return [i for i in digits_lst if (len(i) == 6 and i != nine_pattern and i != zero_pattern)][0]

def id_7(digits_lst):
    return [i for i in digits_lst if len(i) == 3][0]

def id_8(digits_lst):
    return [i for i in digits_lst if len(i) == 7][0]

def id_9(digits_lst):
    four_letters = list(id_4(digits_lst))
    return [i for i in digits_lst if (len(i) == 6 and all(j in i for j in four_letters))][0]

def id_pattern(digits_lst, number):
    match number:
        case 0:
            return id_0(digits_lst)
        case 1:
            return id_1(digits_lst)
        case 4:
            return id_4(digits_lst)
        case 6:
            return id_6(digits_lst)
        case 7:
            return id_7(digits_lst)
        case 8:
            return id_8(digits_lst)
        case 9:
            return id_9(digits_lst)
        case _:
            return "unknown"

def id_patterns(entry):
    sps, ovs = split_entry(entry)
    return {str(i) : id_pattern(sps, i) for i in range(10)}

def read_output(entry):
    sps, ovs = split_entry(entry)
    patterns = id_patterns(entry)
    for number, pattern in patterns.items():
        ovs = [number if i == pattern else i for i in ovs]
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
