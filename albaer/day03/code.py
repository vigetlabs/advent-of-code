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

# Part 1

def bin_str_to_int(bin_str):
    return int(bin_str, 2)

def most_common_bit_at(lst, n):
    bits_at_position = [int(i[n]) for i in lst]
    ones = sum(bits_at_position)
    zeroes = len(lst) - ones
    return 1 if ones >= zeroes else 0

def most_common_bits(lst):
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

# Oxygen Generator Rating

def filter_by_most_common_bit_at(lst, n):
    most_common_n_bit = most_common_bit_at(lst, n)
    return [i for i in lst if i[n] == str(most_common_n_bit)]

def find_ox_gen_rating(lst, index):
    if len(lst) == 1:
        return lst[0]
    elif index > number_of_bits - 1:
        return "ERROR"
    else:
        new_lst = filter_by_most_common_bit_at(lst, index)
        return find_ox_gen_rating(new_lst, index + 1)

def calculate_ox_gen_rating(lst, index):
    bin_str = find_ox_gen_rating(input_lines, 0)
    return bin_str_to_int(bin_str)

# C02 Scrubber Rating

def least_common_bit_at(lst, n):
    bits_at_position = [int(i[n]) for i in lst]
    ones = sum(bits_at_position)
    zeroes = len(lst) - ones
    return 1 if ones < zeroes else 0

def filter_by_least_common_bit_at(lst, n):
    least_common_n_bit = least_common_bit_at(lst, n)
    return [i for i in lst if i[n] == str(least_common_n_bit)]

def find_co2_scrubber_rating(lst, index):
    if len(lst) == 1:
        return lst[0]
    elif index > number_of_bits - 1:
        return "ERROR"
    else:
        new_lst = filter_by_least_common_bit_at(lst, index)
        return find_co2_scrubber_rating(new_lst, index + 1)

def calculate_co2_scrubber_rating(lst, index):
    bin_str = find_co2_scrubber_rating(input_lines, 0)
    return bin_str_to_int(bin_str)

co2_scrubber_rating = calculate_co2_scrubber_rating(input_lines, 0)

def calculate_life_support_rating(lst):
    ox_gen_rating = calculate_ox_gen_rating(lst, 0)
    co2_scrubber_rating = calculate_co2_scrubber_rating(lst, 0)
    return ox_gen_rating * co2_scrubber_rating

part_2_result = calculate_life_support_rating(input_lines)

# Write solution

solution = str(part_1_result) + "\n" + str(part_2_result)
write_solution(solution)
