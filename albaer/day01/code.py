# from pprint import pprint

input_file_path = "input.txt"
output_file_path = "solution.txt"

with open(input_file_path, "r") as file:
    total = 0
    input_lines = file.read().splitlines()

    for n in range(len(input_lines) - 1):
        previous_measurement = int(input_lines[n])
        current_measurement = int(input_lines[n + 1])
        increase = previous_measurement < current_measurement

        if increase:
            total +=1

with open(output_file_path, "w") as solution:
    solution.write(str(total))
