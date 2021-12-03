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

start = [0,0]

for change in input_lines:


write_solution(total_result)

# TODO: Refactor things used every day into shareable code
# (reading input anf writing the solution)
