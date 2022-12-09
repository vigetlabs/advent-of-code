/*
--- Day 7: No Space Left On Device ---
You can hear birds chirping and raindrops hitting leaves as the expedition proceeds. Occasionally, you can even hear much louder sounds in the distance; how big do the animals get out here, anyway?

The device the Elves gave you has problems with more than just its communication system. You try to run a system update:

$ system-update --please --pretty-please-with-sugar-on-top
Error: No space left on device
Perhaps you can delete some files to make space for the update?

You browse around the filesystem to assess the situation and save the resulting terminal output (your puzzle input). For example:

$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k
The filesystem consists of a tree of files (plain data) and directories (which can contain other directories or files). The outermost directory is called /. You can navigate around the filesystem, moving into or out of directories and listing the contents of the directory you're currently in.

Within the terminal output, lines that begin with $ are commands you executed, very much like some modern computers:

cd means change directory. This changes which directory is the current directory, but the specific result depends on the argument:
cd x moves in one level: it looks in the current directory for the directory named x and makes it the current directory.
cd .. moves out one level: it finds the directory that contains the current directory, then makes that directory the current directory.
cd / switches the current directory to the outermost directory, /.
ls means list. It prints out all of the files and directories immediately contained by the current directory:
123 abc means that the current directory contains a file named abc with size 123.
dir xyz means that the current directory contains a directory named xyz.
Given the commands and output in the example above, you can determine that the filesystem looks visually like this:

- / (dir)
  - a (dir)
    - e (dir)
      - i (file, size=584)
    - f (file, size=29116)
    - g (file, size=2557)
    - h.lst (file, size=62596)
  - b.txt (file, size=14848514)
  - c.dat (file, size=8504156)
  - d (dir)
    - j (file, size=4060174)
    - d.log (file, size=8033020)
    - d.ext (file, size=5626152)
    - k (file, size=7214296)
Here, there are four directories: / (the outermost directory), a and d (which are in /), and e (which is in a). These directories also contain files of various sizes.

Since the disk is full, your first step should probably be to find directories that are good candidates for deletion. To do this, you need to determine the total size of each directory. The total size of a directory is the sum of the sizes of the files it contains, directly or indirectly. (Directories themselves do not count as having any intrinsic size.)

The total sizes of the directories above can be found as follows:

The total size of directory e is 584 because it contains a single file i of size 584 and no other directories.
The directory a has total size 94853 because it contains files f (size 29116), g (size 2557), and h.lst (size 62596), plus file i indirectly (a contains e which contains i).
Directory d has total size 24933642.
As the outermost directory, / contains every file. Its total size is 48381165, the sum of the size of every file.
To begin, find all of the directories with a total size of at most 100000, then calculate the sum of their total sizes. In the example above, these directories are a and e; the sum of their total sizes is 95437 (94853 + 584). (As in this example, this process can count files more than once!)

Find all of the directories with a total size of at most 100000. What is the sum of the total sizes of those directories?
*/

// 1. calculate a file size for a directory (size of files + size of subdirectories)

use std::{
    collections::HashMap,
    fmt::{self, Display, Formatter},
};
use uuid::Uuid;

// let id = Uuid::new_v4();

#[derive(Debug, Clone)]
struct File {
    id: Uuid,
    parent_id: Option<Uuid>,
    name: String,
    kind: Option<String>, // eot, jpg, txt
    size: usize,
}

impl File {
    pub fn new(name: String, size: usize, kind: Option<String>, parent_id: Option<Uuid>) -> Self {
        File {
            id: Uuid::new_v4(),
            parent_id,
            name,
            kind,
            size,
        }
    }
}

impl Display for File {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if let Some(ext) = &self.kind {
            write!(f, "{}.{} ({}) [{}]", self.name, ext, self.size, self.id)
        } else {
            write!(f, "{} ({}) [{}]", self.name, self.size, self.id)
        }
    }
}

#[derive(Debug, Clone)]
pub struct Dir {
    id: Uuid,
    parent_id: Option<Uuid>,
    name: String,
    dirs: Vec<Uuid>,
    files: Vec<Uuid>,
}

impl Dir {
    pub fn new(name: &str, parent_id: Option<Uuid>) -> Self {
        Dir {
            id: Uuid::new_v4(),
            parent_id,
            name: name.to_string(),
            dirs: vec![],
            files: vec![],
        }
    }

    fn subdir(&self, id: Uuid) -> Option<Uuid> {
        self.dirs.into_iter().find(|d| d == &id)
    }

    fn add_dir(&mut self, dir: &Dir) {
        println!("Add dir: {:?}", dir);
        self.dirs.push(dir.id);
    }

    fn add_file(&mut self, file: &File) {
        println!("Add file: {}", file);
        self.files.push(file.id);
    }
}

enum Location<'a> {
    F(&'a File),
    D(&'a Dir),
}

fn file_size(fs: HashMap<Uuid, Location>, id: &Uuid) -> usize {
    match fs.get(&id) {
        Some(Location::D(dir)) => {
            let dirs_size: usize = dir.files.iter().map(|id| file_size(fs, id)).sum();
            let files_size: usize = dir.dirs.iter().map(|id| file_size(fs, id)).sum();

            dirs_size + files_size
        }
        Some(Location::F(file)) => file.size,
        _ => panic!("And I oop"),
    }
}

struct FileSystem<'a> {
    locs: HashMap<Uuid, Location<'a>>,
    names: HashMap<String, Uuid>,
}

impl<'a> FileSystem<'a> {
    pub fn new() -> Self {
        FileSystem {
            locs: HashMap::new(),
            names: HashMap::new(),
        }
    }

    fn add(&mut self, loc: &Location) {
        match loc {
            Location::F(file) => self.locs.insert(file.id, *loc),
            Location::D(dir) => self.locs.insert(dir.id, *loc),
        };
    }

    fn get(&self, id: Uuid) -> Option<&Location> {
        self.locs.get(&id)
    }

    fn get_mut(&mut self, id: Uuid) -> Option<&mut Location> {
        self.locs.get_mut(&id)
    }

    fn get_by_name(&self, name: String) -> Option<&Location> {
        self.names.get(&name).and_then(|id| self.locs.get(id))
    }

    fn get_by_name_mut(&mut self, name: String) -> Option<&mut Location> {
        self.names.get(&name).and_then(|id| self.locs.get_mut(id))
    }
}

#[aoc_generator(day7)]
pub fn input_generator(input: &str) -> Dir {
    let mut fs = FileSystem::new();
    let root_dir = Dir::new("/", None);

    fs.add(&Location::D(&root_dir));

    let mut cwd = root_dir.id.clone();

    for line in input.lines() {
        let mut words = line.split_whitespace().peekable();

        // command
        //
        // $ cd <dir>, $ cd ..
        // $ ls
        // dir <dir>  -  the current dir has a dir named <dir>
        // <size> <name> - the current dir has a file named <name> with size <size>
        if let Some(&"$") = words.peek() {
            let _ = words.next();

            match words.next() {
                Some("cd") => {
                    match words.next() {
                        Some("..") => {
                            // go up
                        }
                        Some(name) => {
                            if let Some(Location::D(subdir)) = fs.get_by_name(name.to_string()) {
                                // dir exists, switch to it
                                cwd = subdir.id.clone();
                            } else {
                                let dir = Dir::new(name, Some(cwd));

                                fs.add(&Location::D(&dir));

                                cwd = dir.id.clone();
                            }
                        }
                        None => panic!("Expected directory name to `cd` into"),
                    }
                }
                Some("ls") => {}
                Some(cmd) => panic!("{}", format!("Unrecognized command, '{}'!", cmd)),
                _ => panic!("Invalid input line"),
            }

            continue;
        }

        // there's a dir
        if let Some(&"dir") = words.peek() {
            continue;
        }

        // there's a file
        if let Some(_) = words.peek() {
            let size = words
                .next()
                .expect("Invalid file")
                .parse::<usize>()
                .expect("Invalid file size");
            let full_name = words.next().expect("Invalid file");

            let (filename, kind) = if let Some(_) = full_name.find(".") {
                let (filename, ext) = full_name.split_once(".").expect("Invalid file name");
                (filename.to_string(), Some(ext.to_string()))
            } else {
                (full_name.to_string(), None)
            };

            fs.add(&Location::F(&File::new(filename, size, kind, Some(cwd))));
        }
    }

    root_dir
}

#[aoc(day7, part1)]
pub fn part1(input: &Dir) -> usize {
    0
}

#[aoc(day7, part2)]
pub fn part2(input: &Dir) -> usize {
    0
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn input() {
        let input = "$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k";
        let root = input_generator(input);

        assert_eq!(root.dirs.len(), 2);
        assert_eq!(root.files.len(), 2);
        assert_eq!(root.file_size(), 48381165);
    }

    #[test]
    fn sample1() {
        let input = "";
        assert_eq!(part1(&input_generator(input)), 0);
    }

    #[test]
    fn sample2() {
        let input = "";
        assert_eq!(part2(&input_generator(input)), 0);
    }
}
