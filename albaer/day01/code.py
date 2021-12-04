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

# Part 1

inputs_as_integers = [int(i) for i in read_input_lines()]
pairs = consecutive_groups(inputs_as_integers, 2)
part_1_result = sum([1 for pair in pairs if pair[0] < pair[1]])

# Part 2

triplets = consecutive_groups(inputs_as_integers, 3)
triplet_sums = [sum(triplet) for triplet in triplets]
pairs_of_sums = consecutive_groups(triplet_sums, 2)
part_2_result = sum([1 for pair in pairs_of_sums if pair[0] < pair[1]])

solution = str(part_1_result) + "\n" + str(part_2_result)
write_solution(solution)
