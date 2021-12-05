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

def bin_str_to_decimal(bin_str):
    return int(bin_str, 2)

def bits_to_bin_str(bits):
    return "".join(bits)

def reverse_bits(lst):
    return ["1" if i == "0" else "0" for i in lst]

def bits_to_int(bits):
    bin_str = bits_to_bin_str(bits)
    return bin_str_to_decimal(bin_str)

def most_common_bits(lst):
    number_of_bits = len(lst[0])
    return [most_common_bit_at(lst, i) for i in range(number_of_bits)]

def least_common_bits(lst):
    most_common = most_common_bits(lst)
    return reverse_bits(most_common)

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

# def find_by_prevalence(lst, prevalence, tie_goes_to):
#     bits_at_position = [int(i[n]) for i in lst]
#     ones = lst.count()
#     zeroes = len(lst) - ones
#     match prevalence:
#         case "highest":

def count_bits_at(lst, n):
    bits_at_position = [i[n] for i in lst]
    ones = bits_at_position.count("1")
    zeroes = bits_at_position.count("0")
    return [ones, zeroes]

def most_common_bit_at(lst, n):
    ones, zeroes = count_bits_at(lst, n)
    return "1" if ones >= zeroes else "0"

def least_common_bit_at(lst, n):
    ones, zeroes = count_bits_at(lst, n)
    return "1" if ones < zeroes else "0"

def filter_by_prevalence_at(lst, position, prevalence):
    match prevalence:
        case "highest":
            desired_bit = most_common_bit_at(lst, position)
        case "lowest":
            desired_bit = least_common_bit_at(lst, position)

    return [i for i in lst if i[position] == desired_bit]


# Part 2

def find_by_prevalence(lst, index, prevalence):
    number_of_bits = len(lst[0])
    if len(lst) == 1:
        return lst[0]
    elif index > number_of_bits - 1:
        return "ERROR"
    else:
        new_lst = filter_by_prevalence_at(lst, index, prevalence)
        return find_by_prevalence(new_lst, index + 1, prevalence)

# Oxygen Generator Rating

def calculate_ox_gen_rating(lst):
    bin_str = find_by_prevalence(lst, 0, "highest")
    return bin_str_to_decimal(bin_str)

# C02 Scrubber Rating

def calculate_co2_scrubber_rating(lst):
    bin_str = find_by_prevalence(lst, 0, "lowest")
    return bin_str_to_decimal(bin_str)

# Life Support rating

def calculate_life_support_rating(lst):
    ox_gen_rating = calculate_ox_gen_rating(lst)
    co2_scrubber_rating = calculate_co2_scrubber_rating(lst)
    return ox_gen_rating * co2_scrubber_rating

# Write solution

if __name__ == '__main__':
    input_lines = read_input_lines()
    part_1_result = calculate_power_consumption(input_lines)
    part_2_result = calculate_life_support_rating(input_lines)
    solution = str(part_1_result) + "\n" + str(part_2_result)
    write_solution(solution)
