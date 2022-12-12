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

use std::{
    collections::HashMap,
    fmt::{self, Display, Formatter},
};
use uuid::Uuid;

#[derive(Debug, Clone)]
struct FileRecord {
    id: Uuid,
    parent_id: Option<Uuid>,
    name: String,
    kind: Option<String>, // eot, jpg, txt
    size: usize,
}

#[derive(Debug, Clone)]
pub struct DirectoryRecord {
    id: Uuid,
    parent_id: Option<Uuid>,
    name: String,
    dirs: Vec<Uuid>,
    files: Vec<Uuid>,
}

#[derive(Debug)]
enum Location {
    File(Uuid, FileRecord),
    Dir(Uuid, DirectoryRecord),
}

#[derive(Debug)]
pub struct FileSystem {
    cwd: Option<Uuid>,
    locs: HashMap<Uuid, Location>,
    names: HashMap<String, Uuid>,
}

impl FileRecord {
    pub fn new(
        id: Uuid,
        name: String,
        size: usize,
        kind: Option<String>,
        parent_id: Option<Uuid>,
    ) -> Self {
        FileRecord {
            id,
            parent_id,
            name,
            kind,
            size,
        }
    }
}

impl Display for FileRecord {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if let Some(ext) = &self.kind {
            write!(f, "{}.{} ({}) [{}]", self.name, ext, self.size, self.id)
        } else {
            write!(f, "{} ({}) [{}]", self.name, self.size, self.id)
        }
    }
}

impl DirectoryRecord {
    pub fn new(id: Uuid, name: &str, parent_id: Option<Uuid>) -> Self {
        DirectoryRecord {
            id,
            parent_id,
            name: name.to_string(),
            dirs: vec![],
            files: vec![],
        }
    }

    fn subdir(&self, id: Uuid) -> Option<Uuid> {
        self.dirs.clone().into_iter().find(|d| d == &id)
    }

    fn add_dir(&mut self, dir_id: Uuid) {
        self.dirs.push(dir_id);
    }

    fn add_file(&mut self, file_id: Uuid) {
        self.files.push(file_id);
    }
}

fn loc_id(loc: &Location) -> Uuid {
    match loc {
        Location::File(id, _) => *id,
        Location::Dir(id, _) => *id,
    }
}

// fn full_path(fs: &FileSystem, cwd_id: &Uuid, name: &str) -> String {}

pub fn file_size(fs: &FileSystem, id: &Uuid) -> usize {
    match fs.get(id) {
        Some(Location::Dir(_, dir)) => {
            let dirs_size: usize = dir.dirs.iter().map(|id| file_size(fs, id)).sum();
            let files_size: usize = dir.files.iter().map(|id| file_size(fs, id)).sum();

            dirs_size + files_size
        }
        Some(Location::File(_, file)) => file.size,
        _ => panic!("And I oop"),
    }
}

impl FileSystem {
    pub fn new() -> Self {
        FileSystem {
            cwd: None,
            locs: HashMap::new(),
            names: HashMap::new(),
        }
    }

    fn cd(&mut self, loc_id: Option<&Uuid>) -> Result<(), ()> {
        match loc_id {
            Some(id) => {
                self.cwd = Some(*id);
                Ok(())
            }
            None => Err(()),
        }
    }

    fn try_cd_by_name(&mut self, name: String) -> Result<(), ()> {
        self.names
            .get(&name)
            .and_then(|id| {
                self.cwd = Some(*id);
                Some(())
            })
            .ok_or(())
    }

    fn add(&mut self, loc: Location) {
        match loc {
            Location::Dir(id, ref dir) => {
                self.names.insert(dir.name.to_string(), id);
                self.locs.insert(loc_id(&loc), loc);
            }
            Location::File(id, ref file) => {
                self.names.insert(file.name.to_string(), id);
                self.locs.insert(loc_id(&loc), loc);
            }
        }
    }

    fn get(&self, id: &Uuid) -> Option<&Location> {
        self.locs.get(id)
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
pub fn input_generator(input: &str) -> FileSystem {
    let mut fs = FileSystem::new();
    // let mut full_path = vec![];

    for line in input.lines() {
        let mut words = line.split_whitespace().peekable();

        /*
         * parse command from line
         *      $ cd <dir>, $ cd ..
         *      $ ls
         */
        if let Some(&"$") = words.peek() {
            let _ = words.next();

            match words.next() {
                Some("cd") => {
                    match words.next() {
                        // go up
                        Some("..") => {
                            if let Some(Location::Dir(_, dir)) = fs.get(&fs.cwd.unwrap()) {
                                fs.cd(Some(&dir.parent_id.unwrap())).ok();
                            }
                        }

                        // cd into dir (and create it if not pre-existing)
                        Some(name) => {
                            fs.try_cd_by_name(name.to_string())
                                .map_err(|r| {
                                    let id = Uuid::new_v4();
                                    let dir = DirectoryRecord::new(id, name, fs.cwd);
                                    let dir_loc = Location::Dir(id, dir);

                                    fs.add(dir_loc);
                                    fs.cd(Some(&id)).ok();
                                })
                                .ok();
                        }

                        None => panic!("Expected directory name to `cd` into"),
                    }
                }

                // list cwd contents
                Some("ls") => {
                    // do nothing
                }

                // unrecognized cmd
                Some(cmd) => panic!("{}", format!("Unrecognized command, '{}'!", cmd)),

                // unexpected end of input
                _ => panic!("Invalid input line"),
            }

            continue;
        }

        /*
         * cwd contains a directory with the specified name
         *      dir <dir>
         */
        if let Some(&"dir") = words.peek() {
            let _ = words.next();

            if let Some(name) = words.next() {
                let dir_id = Uuid::new_v4();
                let dir = DirectoryRecord::new(dir_id, name, fs.cwd);
                let dir_loc = Location::Dir(dir_id, dir);

                fs.add(dir_loc);

                if let Some(Location::Dir(_, cwd_dir)) = fs.get_mut(fs.cwd.unwrap()) {
                    cwd_dir.add_dir(dir_id);
                }
            }

            continue;
        }

        /*
         * cwd has a file with the specified name and size
         *      <size> <name>
         */
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

            let file_id = Uuid::new_v4();
            let file = FileRecord::new(file_id, filename, size, kind, fs.cwd);

            fs.add(Location::File(file.id, file));

            if let Some(Location::Dir(_, cwd_dir)) = fs.get_mut(fs.cwd.unwrap()) {
                cwd_dir.add_file(file_id);
            }
        }
    }

    fs
}

#[aoc(day7, part1)]
pub fn part1(fs: &FileSystem) -> usize {
    let mut total_size = 0;

    for (id, loc) in fs.locs.iter() {
        if let Location::Dir(_, dir) = loc {
            let size = file_size(fs, &id);

            if size <= 100000 {
                total_size += size;
            }
        }
    }

    total_size
}

#[aoc(day7, part2)]
pub fn part2(fs: &FileSystem) -> usize {
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
        let fs = input_generator(input);
        println!("{:#?}", fs);
        assert_eq!(fs.locs.len(), 14);
        if let Location::Dir(id, _) = fs.get_by_name("/".to_string()).unwrap() {
            assert_eq!(file_size(&fs, id), 48381165);
            // assert_eq!(true, false);
        } else {
            panic!("Could not locate root dir");
        }
    }

    #[test]
    fn sample1() {
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
        let fs = input_generator(input);
        assert_eq!(part1(&fs), 95437);
    }

    #[test]
    fn sample2() {
        let input = "";
        assert_eq!(part2(&input_generator(input)), 0);
    }
}
