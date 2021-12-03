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

# Part 1

def bin_str_to_int(bin_str):
    return int(bin_str, 2)

def most_common_bit_at(lst, n):
    bits_at_position = [int(i[n]) for i in lst]
    ones = sum(bits_at_position)
    zeroes = len(lst) - ones
    return 1 if ones >= zeroes else 0

def most_common_bits(lst):
    number_of_bits = len(lst[0])
    return [str(most_common_bit_at(lst, i)) for i in range(number_of_bits)]

def least_common_bits(lst):
    most_common = most_common_bits(lst)
    return reverse_bits(most_common)

def bits_to_bin_str(bits):
    return "".join(bits)

def reverse_bits(lst):
    return ["1" if i == "0" else "0" for i in lst]

def bits_to_int(bits):
    bin_str = bits_to_bin_str(bits)
    return bin_str_to_int(bin_str)

def calculate_gamma(lst):
    bits = most_common_bits(lst)
    return bits_to_int(bits)

def calculate_epsilon(lst):
    bits = least_common_bits(lst)
    return bits_to_int(bits)

def calculate_power_consumption(lst):
    gamma = calculate_gamma(lst)
    epsilon = calculate_epsilon(lst)
    return gamma * epsilon

part_1_result = calculate_power_consumption(input_lines)

# Part 2

part_2_result = 0

# Write solution

solution = str(part_1_result) + "\n" + str(part_2_result)
write_solution(solution)
