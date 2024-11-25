import unittest
from code import *

class TestDay06(unittest.TestCase):
    EXAMPLE_INPUT = ["3","4","3","1","2",]

    # Part 1

    def test_represent_population_after_0_days(self):
        actual = represent_population(self.EXAMPLE_INPUT, 0)
        expected = {'1': 1, '2': 1, '3': 2, '4': 1}
        self.assertEqual(actual, expected)

    def test_represent_population_after_5_days(self):
        actual = represent_population(self.EXAMPLE_INPUT, 5)
        expected = {'3': 1, '4': 1, '5': 3, '6': 2, '7': 2, '8': 1}
        self.assertEqual(actual, expected)

    def test_represent_population_after_18_days(self):
        actual = represent_population(self.EXAMPLE_INPUT, 18)
        expected = {'0': 3, '1': 5, '2': 3, '3': 2, '4': 2, '6': 5, '7': 1, '5': 1, '8': 4}
        self.assertEqual(actual, expected)

    def test_count_population_after_0_days(self):
        actual = count_population(self.EXAMPLE_INPUT, 0)
        expected = 5
        self.assertEqual(actual, expected)

    def test_count_population_after_18_days(self):
        actual = count_population(self.EXAMPLE_INPUT, 18)
        expected = 26
        self.assertEqual(actual, expected)

    def test_count_population_after_80_days(self):
        actual = count_population(self.EXAMPLE_INPUT, 80)
        expected = 5934
        self.assertEqual(actual, expected)

    # Part 2

    def test_count_population_after_256_days(self):
        actual = count_population(self.EXAMPLE_INPUT, 256)
        expected = 26984457539
        self.assertEqual(actual, expected)

if __name__ == '__main__':
    unittest.main()

