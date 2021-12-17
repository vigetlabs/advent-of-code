import unittest
from code import *

class TestDay12(unittest.TestCase):
    EXAMPLE_INPUT_A = [
        "start-A",
        "start-b",
        "A-c",
        "A-b",
        "b-d",
        "A-end",
        "b-end",
    ]

    EXAMPLE_INPUT_B = [
        "dc-end",
        "HN-start",
        "start-kj",
        "dc-start",
        "dc-HN",
        "LN-dc",
        "HN-end",
        "kj-sa",
        "kj-HN",
        "kj-dc",
    ]

    EXAMPLE_INPUT_C = [
        "fs-end",
        "he-DX",
        "fs-he",
        "start-DX",
        "pj-DX",
        "end-zg",
        "zg-sl",
        "zg-pj",
        "pj-he",
        "RW-he",
        "fs-DX",
        "pj-RW",
        "zg-RW",
        "start-pj",
        "he-WI",
        "zg-he",
        "pj-fs",
        "start-RW",
    ]

    # Part 1

    def test_count_distinct_paths_a(self):
        actual = count_distinct_paths(self.EXAMPLE_INPUT_A, valid_part_1)
        expected = 10
        self.assertEqual(actual, expected)

    def test_count_distinct_paths_b(self):
        actual = count_distinct_paths(self.EXAMPLE_INPUT_B, valid_part_1)
        expected = 19
        self.assertEqual(actual, expected)

    def test_count_distinct_paths_c(self):
        actual = count_distinct_paths(self.EXAMPLE_INPUT_C, valid_part_1)
        expected = 226
        self.assertEqual(actual, expected)

    # Part 2

    # def test_count_distinct_paths_a(self):
    #     actual = count_distinct_paths(self.EXAMPLE_INPUT_A)
    #     expected = 36
    #     self.assertEqual(actual, expected)

    # def test_count_distinct_paths_b(self):
    #     actual = count_distinct_paths(self.EXAMPLE_INPUT_B)
    #     expected = 103
    #     self.assertEqual(actual, expected)

    # def test_count_distinct_paths_c(self):
    #     actual = count_distinct_paths(self.EXAMPLE_INPUT_C)
    #     expected = 3509
    #     self.assertEqual(actual, expected)

if __name__ == '__main__':
    unittest.main()

