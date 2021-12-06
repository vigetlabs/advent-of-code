import unittest
from code import *

class TestDay06(unittest.TestCase):
    EXAMPLE_INPUT = [3,4,3,1,2,]

    # Part 1

    def test_represent_population_after_5_days(self):
        actual = represent_population(self.EXAMPLE_INPUT, 5)
        expected = [5,6,5,3,4,5,6,7,7,8,]
        self.assertEqual(actual, expected)

    def test_represent_population_after_18_days(self):
        actual = represent_population(self.EXAMPLE_INPUT, 18)
        expected = [6,0,6,4,5,6,0,1,1,2,6,0,1,1,1,2,2,3,3,4,6,7,8,8,8,8]
        self.assertEqual(actual, expected)

    def test_count_population_after_18_days(self):
        actual = count_population(self.EXAMPLE_INPUT, 18)
        expected = 26
        self.assertEqual(actual, expected)

    def test_count_population_after_80_days(self):
        actual = count_population(self.EXAMPLE_INPUT, 80)
        expected = 5934
        self.assertEqual(actual, expected)

if __name__ == '__main__':
    unittest.main()

