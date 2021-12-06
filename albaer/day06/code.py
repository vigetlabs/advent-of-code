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

def represent_population(lst, days):
    [i + days for i in lst]

def count_population(lst, days):
    len(represent_population(lst, days))
# Part 2

# Write solution

if __name__ == '__main__':
    # input_lines = read_input_lines()
    part_1_result = "TODO"
    part_2_result = "TODO"
    solution = str(part_1_result) + "\n" + str(part_2_result)
    write_solution(solution)
