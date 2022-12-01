def read_integer_puzzle_input(file_name):
  ret = []
  with open(file_name) as f:
    for line in f:
      ret.append(int(line))
  return ret

def read_string_puzzle_input(file_name):
  ret = []
  with open(file_name) as f:
    for line in f:
      ret.append(line)
  return ret
