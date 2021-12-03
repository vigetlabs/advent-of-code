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

input_lines = read_input_lines()
number_of_bits = len(input_lines[0])

pprint(number_of_bits)
pprint(range(number_of_bits))

def most_common_at(n):
    bits_at_position = [int(i[n]) for i in input_lines]
    ones = sum(bits_at_position)
    zeroes = len(input_lines) - ones
    return 1 if ones > zeroes else 0

binary_digits = [str(most_common_at(i)) for i in range(number_of_bits)]
binary_string = "".join(binary_digits)
pprint(binary_string)

gamma = int(binary_string, 2)
pprint(gamma)

# Part 1

part_1_result = 0

# Part 2

part_2_result = 0

# Write solution

solution = str(part_1_result) + "\n" + str(part_2_result)
write_solution(solution)
