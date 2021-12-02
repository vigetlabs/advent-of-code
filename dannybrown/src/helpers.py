def read_puzzle_input(file_name):
  ret = []
  with open(file_name) as f:
    for line in f:
      ret.append(int(line))
  return ret
