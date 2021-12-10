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

# Pattern comparison methods

def alphabetize(string):
    return "".join(sorted(string))

def same_patterns(p1, p2):
    return alphabetize(p1) == alphabetize(p2)

def different_patterns(p1, p2):
    return not(same_patterns(p1, p2))

def subset_pattern(p1, p2):
    return all(i in p2 for i in list(p1))

def get_output_values(entries_lst):
    entries = get_entries(entries_lst)
    return [i[1] for i in entries]

# Skipping all the hard stuff for part 1

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

# TODO: Consolidate these with search/find method
# TODO: Pass around the incomplete patterns dict instead of repeating calculations

def id_0(digits_lst):
    nine = id_9(digits_lst)
    one = id_1(digits_lst)
    return [i for i in digits_lst if (len(i) == 6 and different_patterns(i, nine) and subset_pattern(one, i))][0]

def id_1(digits_lst):
    return [i for i in digits_lst if len(i) == 2][0]

def id_2(digits_lst):
    three = id_3(digits_lst)
    five = id_5(digits_lst)
    return [i for i in digits_lst if (len(i) == 5 and different_patterns(i, three) and different_patterns(i, five))][0]

def id_3(digits_lst):
    one = id_1(digits_lst)
    return [i for i in digits_lst if (len(i) == 5 and subset_pattern(one, i))][0]

def id_4(digits_lst):
    return [i for i in digits_lst if len(i) == 4][0]

def id_5(digits_lst):
    three = id_3(digits_lst)
    nine = id_9(digits_lst)
    return [i for i in digits_lst if (len(i) == 5 and different_patterns(i, three) and subset_pattern(i, nine))][0]

def id_6(digits_lst):
    nine = id_9(digits_lst)
    zero_pattern = id_0(digits_lst)
    return [i for i in digits_lst if (len(i) == 6 and different_patterns(i, nine) and different_patterns(i, zero_pattern))][0]

def id_7(digits_lst):
    return [i for i in digits_lst if len(i) == 3][0]

def id_8(digits_lst):
    return [i for i in digits_lst if len(i) == 7][0]

def id_9(digits_lst):
    four = id_4(digits_lst)
    return [i for i in digits_lst if (len(i) == 6 and subset_pattern(four, i))][0]

def id_pattern(digits_lst, number):
    function_name = "id_" + str(number)
    return eval(function_name)(digits_lst)

def id_patterns(entry):
    sps, ovs = split_entry(entry)
    result = {id_pattern(sps, i): str(i) for i in range(10)}
    return result

def read_output(entry):
    sps, ovs = split_entry(entry)
    patterns = id_patterns(entry)
    normalized_ovs = [alphabetize(i) for i in ovs]
    normalized_patterns = {alphabetize(k): v for k, v in patterns.items()}
    replaced_output = [normalized_patterns[i] for i in normalized_ovs]
    result = int("".join([str(i) for i in replaced_output]))
    return result

def sum_output_values(entries_lst):
    outputs = [read_output(i) for i in entries_lst]
    return sum(outputs)

# Write solution

if __name__ == '__main__':
    entries = read_input_lines()
    part_1_result = count_1_4_7_8(entries)
    part_2_result = sum_output_values(entries)
    solution = str(part_1_result) + "\n" + str(part_2_result)
    write_solution(solution)
