from pprint import pprint
from collections import Counter

def write_solution(solution, output_file_path="solution.txt"):
    with open(output_file_path, "w") as output_file:
        output_file.write(str(solution))
        print(solution)
        return solution

def read_input_lines(input_file_path="input.txt"):
    with open(input_file_path, "r") as input_file:
        return input_file.read().splitlines()

def split_inputs_from_first_line():
    input_lines = read_input_lines()
    first_line_as_str = input_lines[0]
    return first_line_as_str.split(",")

# Utilities

def flatten(lst):
    return [item for sublist in lst for item in sublist]

# Part 1

def other_cave(tunnel, cave):
    return tunnel.replace(cave, "").replace("-", "")

def caves_connected_to(tunnels, cave):
    return [other_cave(tunnel, cave) for tunnel in tunnels if cave in tunnel]

def add_cave_to_path_list(path_list, cave):
    new_path_list = path_list.copy()
    new_path_list.append(cave)
    return new_path_list

def paths_from(tunnels, path_list, valid):
    last_cave = path_list[-1]
    possible_next_caves = caves_connected_to(tunnels, last_cave)
    valid_next_caves = [cave for cave in possible_next_caves if valid(path_list, cave)]
    return [add_cave_to_path_list(path_list, cave) for cave in valid_next_caves]

def valid_part_1(path_list, cave):
    if cave == "start":
        return False
    elif cave.islower() and cave in path_list:
        return False
    else:
        return True

def valid_part_2(path_list, cave):
    small_caves = [cave for cave in path_list if cave.islower()]
    already_visited_small_twice = [k for k, v in Counter(small_caves).items() if v > 1] != []
    returning_to_small_cave = cave.islower() and cave in path_list
    if cave == "start":
        return False
    elif already_visited_small_twice and returning_to_small_cave:
        return False
    else:
        return True

def find_paths(tunnels, valid, paths=[["start"]], complete_paths=[], ):
    if paths == []:
        return complete_paths
    else:
        all_paths = flatten([paths_from(tunnels, path, valid) for path in paths])
        new_complete_paths = complete_paths + [path for path in all_paths if path[-1] == "end"]
        new_paths = [path for path in all_paths if path[-1] != "end"]
        return find_paths(tunnels, valid, new_paths, new_complete_paths)

def count_distinct_paths(tunnels, valid):
    return len(find_paths(tunnels, valid))

# Write solution

if __name__ == '__main__':
    input_tunnels = read_input_lines()
    part_1_result = count_distinct_paths(input_tunnels, valid_part_1)
    part_2_result = count_distinct_paths(input_tunnels, valid_part_2)
    solution = str(part_1_result) + "\n" + str(part_2_result)
    write_solution(solution)
