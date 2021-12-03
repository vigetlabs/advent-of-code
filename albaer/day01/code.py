from functools import reduce
from pprint import pprint

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

# Sort of like ruby's each_cons without the block option
# consecutive_groups([1, 2, 3, 4], 2) returns [[1, 2], [2, 3], [3, 4]]
def consecutive_groups(lst, n):
    return [lst[i:i+n] for i in range(len(lst)-n+1)]

input_lines = read_input_lines()

def check_increase(total, pair):
    increase = int(pair[0]) < int(pair[1])
    return total + 1 if increase else total

total_result = reduce(check_increase, consecutive_groups(input_lines, 2), 0)

write_solution(total_result)

