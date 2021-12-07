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

def inputs_as_integers():
    return [int(i) for i in split_inputs_from_first_line()]

def determine_fuel_cost_to_alignment_at(lst, position):
    return sum([abs(i - position) for i in lst])

def get_positions(lst):
    return range(min(lst), max(lst) + 1)

def get_fuel_costs(lst):
    positions = get_positions(lst)
    return [determine_fuel_cost_to_alignment_at(lst, i) for i in positions]

def determine_cheapest_alignment(lst):
    positions = get_positions(lst)
    fuel_costs = get_fuel_costs(lst)
    return positions[fuel_costs.index(min(fuel_costs))]

def determine_minimum_fuel_cost(lst):
    fuel_costs = get_fuel_costs(lst)
    return min(fuel_costs)

# Write solution

if __name__ == '__main__':
    int_inputs = inputs_as_integers()
    part_1_result = determine_minimum_fuel_cost(int_inputs)
    part_2_result = "TODO"
    solution = str(part_1_result) + "\n" + str(part_2_result)
    write_solution(solution)
