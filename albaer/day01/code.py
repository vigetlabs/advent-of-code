from functools import reduce

input_file_path = "input.txt"
output_file_path = "solution.txt"

def check_increase(total, index):
    previous_measurement = int(input_lines[index - 1])
    current_measurement = int(input_lines[index])
    increase = previous_measurement < current_measurement
    return total + 1 if increase else total

with open(input_file_path, "r") as file:
    input_lines = file.read().splitlines()
    total_result = reduce(check_increase, range(len(input_lines)))

with open(output_file_path, "w") as solution:
    solution.write(str(total_result))
