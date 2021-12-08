from pprint import pprint
import collections

def write_solution(solution, output_file_path="solution.txt"):
    with open(output_file_path, "w") as output_file:
        output_file.write(str(solution))
        print(solution)
        return solution

def read_input_lines(input_file_path="input.txt"):
    with open(input_file_path, "r") as input_file:
        return input_file.read().splitlines()

def get_inputs():
    input_lines = read_input_lines()
    first_line_as_str = input_lines[0]
    return first_line_as_str.split(",")

# Parts 1 and 2

def get_counts_dct(lst):
    counter = collections.Counter(lst)
    return dict(counter)

def increment_day(dct):
    # Subtract 1 for each fish
    dct = {str(int(k) - 1): v for k, v in dct.items()}
    # Count number of fish that are reproducing
    reproducers_count = dct.get("-1", 0)
    # Add one new fish at 8 for each one that is reproducing
    dct["8"] = reproducers_count
    # Combine fish that just reproduced with existing 6s
    dct["6"] = dct.get("6", 0) + reproducers_count
    if '-1' in dct: del dct['-1']
    # Remove keys with value of 0
    dct = {k: v for k, v in dct.items() if v != 0}
    return dct

def increment_days(dct, days, current_day=0):
    if days == current_day:
        return dct
    else:
        new_dct = increment_day(dct)
        return increment_days(new_dct, days, current_day + 1)

def represent_population(lst, days):
    dct = get_counts_dct(lst)
    return increment_days(dct, days)

def count_population(lst, days):
    population_dict = represent_population(lst, days)
    return sum(population_dict.values())

# Write solution

if __name__ == '__main__':
    inputs = get_inputs()
    part_1_result = count_population(inputs, 80)
    part_2_result = count_population(inputs, 256)
    solution = str(part_1_result) + "\n" + str(part_2_result)
    write_solution(solution)
