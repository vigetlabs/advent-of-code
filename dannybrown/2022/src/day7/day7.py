LARGEST_SIZE = 100000
MAX_SPACE = 70000000
SPACE_NEEDED = 30000000
class TreeNode:
    def __init__(self, text, size=0, children=[], parent=None):
        self.text = text
        self.size = size
        self.children = children
        self.parent = parent
    def __str__(self):
        return self.text

    def __repr__(self):
        return self.text

def get_full_path(node):
    # the same directory name can appear multiple times with a different full path
    if not node:
        return ""
    return node.text + "/" + get_full_path(node.parent)

def part1_2(node, dictionary):
    val = node.size + sum([part1_2(child, dictionary) for child in node.children])
    if node.children != [] and node.size == 0:
        path_to_node = get_full_path(node)
        dictionary[path_to_node] = val
    return val

if __name__ == '__main__':
    with open('dannybrown/2022/input/day7.txt', 'r') as f:
        lines = f.readlines()

    root = TreeNode(text="root", size=0, children=[], parent=None)

    lines = lines[1:] # always start with cd root
    dir_dict = {}
    current_node = root
    for line in lines:
        line = line.strip()
        if line.startswith("$"):
            # command
            line = line.split(" ")
            command = line[1]
            if command == "cd":
                # breakpoint()
                if line[2] == "..":
                    current_node = current_node.parent
                    continue
                for child in current_node.children:
                    if child.text == line[2]:
                        current_node = child
                        break
            else: #ls
                pass
        else:
            # result of ls
            line = line.split(" ")
            size=0
            if line[0] != "dir":
                size = int(line[0])
            else:
                dir_dict[line[1]] = 0
            text = line[1]
            new_node = TreeNode(text=text, size=size, children=[], parent=current_node)
            current_node.children.append(new_node)

    part1_sum = 0
    dct = {}
    part1_2(root, dct)

    for key, value in dct.items():
        if value < LARGEST_SIZE:
            part1_sum += value
    print('Part 1:', part1_sum)

    # Part 2

    root_size = dct["root/"]
    used_space = MAX_SPACE - root_size

    dirs_to_delete = []
    for key,value in dct.items():
        if used_space + value >= SPACE_NEEDED:
            dirs_to_delete.append(value)

    print('Part 2:', min(dirs_to_delete))
