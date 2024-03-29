from pprint import pprint

def write_solution(solution, output_file_path="solution.txt"):
    with open(output_file_path, "w") as output_file:
        output_file.write(str(solution))
        print(solution)
        return solution

def read_input_lines(input_file_path="input.txt"):
    with open(input_file_path, "r") as input_file:
        return input_file.read().splitlines()

# Binary conversion utilities

def bin_str_to_decimal(bin_str):
    return int(bin_str, 2)

def bits_to_int(bits):
    return bin_str_to_decimal("".join(bits))

# Part 1

def get_bit_by_prevalence_at(lst, prevalence, position):
    bits_at_position = [i[position] for i in lst]
    ones = bits_at_position.count("1")
    zeroes = bits_at_position.count("0")

    match prevalence:
        case "highest":
            return "1" if ones >= zeroes else "0"
        case "lowest":
            return "1" if ones < zeroes else "0"

def get_bits_by_prevalence(lst, prevalence):
    number_of_bits = len(lst[0])
    return [get_bit_by_prevalence_at(lst, prevalence, i) for i in range(number_of_bits)]


# Part 1 Calculations

def calculate_gamma(lst):
    bits = get_bits_by_prevalence(lst, "highest")
    return bits_to_int(bits)

def calculate_epsilon(lst):
    bits = get_bits_by_prevalence(lst, "lowest")
    return bits_to_int(bits)

def calculate_power_consumption(lst):
    gamma = calculate_gamma(lst)
    epsilon = calculate_epsilon(lst)
    return gamma * epsilon

# Part 2

def filter_by_prevalence_at(lst, prevalence, position):
    desired_bit = get_bit_by_prevalence_at(lst, prevalence, position)
    return [i for i in lst if i[position] == desired_bit]

def find_by_prevalence(lst, prevalence, index=0):
    number_of_bits = len(lst[0])
    if len(lst) == 1:
        return lst[0]
    elif index > number_of_bits - 1:
        return "ERROR"
    else:
        new_lst = filter_by_prevalence_at(lst, prevalence, index)
        return find_by_prevalence(new_lst, prevalence, index + 1)

# Part 2 Calculations

def calculate_ox_gen_rating(lst):
    bin_str = find_by_prevalence(lst, "highest")
    return bin_str_to_decimal(bin_str)

def calculate_co2_scrubber_rating(lst):
    bin_str = find_by_prevalence(lst, "lowest")
    return bin_str_to_decimal(bin_str)

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
