import unittest
from code import *

class TestDay03(unittest.TestCase):
    EXAMPLE_INPUT = [
        "00100",
        "11110",
        "10110",
        "10111",
        "10101",
        "01111",
        "00111",
        "11100",
        "10000",
        "11001",
        "00010",
        "01010",
    ]

    # Part 1

    def test_calculate_gamma(self):
        actual = calculate_gamma(self.EXAMPLE_INPUT)
        expected = 22
        self.assertEqual(actual, expected)

    def test_calculate_epsilon(self):
        actual = calculate_epsilon(self.EXAMPLE_INPUT)
        expected = 9
        self.assertEqual(actual, expected)

    def test_calculate_power_consumption(self):
        actual = calculate_power_consumption(self.EXAMPLE_INPUT)
        expected = 198
        self.assertEqual(actual, expected)

    # Part 2

    def test_calculate_ox_gen_rating(self):
        actual = calculate_ox_gen_rating(self.EXAMPLE_INPUT)
        expected = 23
        self.assertEqual(actual, expected)

    def test_calculate_co2_scrubber_rating(self):
        actual = calculate_co2_scrubber_rating(self.EXAMPLE_INPUT)
        expected = 10
        self.assertEqual(actual, expected)

    def test_calculate_life_support_rating(self):
        actual = calculate_life_support_rating(self.EXAMPLE_INPUT)
        expected = 230
        self.assertEqual(actual, expected)

if __name__ == '__main__':
    unittest.main()
