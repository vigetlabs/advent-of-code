from functools import reduce

input_file_path = "input.txt"
output_file_path = "solution.txt"

def write_solution(solution):
    with open(output_file_path, "w") as output_file:
        output_file.write(str(solution))
        print(solution)
        return solution

def read_input_lines():
    with open(input_file_path, "r") as input_file:
        return input_file.read().splitlines()

input_lines = read_input_lines()

def check_increase(total, index):
    previous_measurement = int(input_lines[index - 1])
    current_measurement = int(input_lines[index])
    increase = previous_measurement < current_measurement
    return total + 1 if increase else total

total_result = reduce(check_increase, range(len(input_lines)))

write_solution(total_result)

# TODO: Part 2
# TODO: Use list comprehensions for funsies
