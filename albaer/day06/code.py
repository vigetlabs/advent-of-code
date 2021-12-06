from pprint import pprint

def write_solution(solution, output_file_path="solution.txt"):
    with open(output_file_path, "w") as output_file:
        output_file.write(str(solution))
        print(solution)
        return solution

def read_input_lines(input_file_path="input.txt"):
    with open(input_file_path, "r") as input_file:
        return input_file.read().splitlines()

# Part 1

def get_inputs_as_integers():
  input_lines = read_input_lines()
  first_line_as_str = input_lines[0]
  str_inputs = first_line_as_str.split(",")
  return [int(i) for i in str_inputs]

def increment_day(lst):
    # pprint(lst)
    subtract_1_lst = [i - 1 for i in lst]
    # pprint(subtract_1_lst)
    count_reproducers = subtract_1_lst.count(-1)
    # pprint(count_reproducers)
    new_fish_lst = [8] * count_reproducers
    # pprint(new_fish_lst)
    reproduce_lst = subtract_1_lst + new_fish_lst
    # pprint(reproduce_lst)
    result = [6 if i == -1 else i for i in reproduce_lst]
    # pprint(result)
    return result

def increment_days(lst, days, current_day=0):
    if days == current_day:
        return lst
    else:
        new_lst = increment_day(lst)
        return increment_days(new_lst, days, current_day + 1)

def represent_population(lst, days):
    return increment_days(lst, days)

def count_population(lst, days):
    return len(represent_population(lst, days))

# Part 2

# Write solution

if __name__ == '__main__':
    input_ints = get_inputs_as_integers()
    part_1_result = count_population(input_ints, 80)
    part_2_result = "TODO"
    solution = str(part_1_result) + "\n" + str(part_2_result)
    write_solution(solution)
