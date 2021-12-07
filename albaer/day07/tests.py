import unittest
from code import *

class TestDay07(unittest.TestCase):
    EXAMPLE_INPUT = [16,1,2,0,4,2,7,1,2,14,]

    # Part 1

    def test_determine_constant_fuel_cost_to_alignment_at_1(self):
        actual = determine_fuel_cost_to_alignment_at(self.EXAMPLE_INPUT, 1, "constant")
        expected = 41
        self.assertEqual(actual, expected)

    def test_determine_constant_fuel_cost_to_alignment_at_2(self):
        actual = determine_fuel_cost_to_alignment_at(self.EXAMPLE_INPUT, 2, "constant")
        expected = 37
        self.assertEqual(actual, expected)

    def test_determine_constant_fuel_cost_to_alignment_at_3(self):
        actual = determine_fuel_cost_to_alignment_at(self.EXAMPLE_INPUT, 3, "constant")
        expected = 39
        self.assertEqual(actual, expected)

    def test_determine_constant_fuel_cost_to_alignment_at_10(self):
        actual = determine_fuel_cost_to_alignment_at(self.EXAMPLE_INPUT, 10, "constant")
        expected = 71
        self.assertEqual(actual, expected)

    def test_determine_constant_cheapest_alignment(self):
        actual = determine_cheapest_alignment(self.EXAMPLE_INPUT, "constant")
        expected = 2
        self.assertEqual(actual, expected)

    def test_determine_constant_minimum_fuel_cost(self):
        actual = determine_minimum_fuel_cost(self.EXAMPLE_INPUT, "constant")
        expected = 37
        self.assertEqual(actual, expected)

    # Part 2

    def test_determine_increasing_fuel_cost_to_alignment_at_2(self):
        actual = determine_fuel_cost_to_alignment_at(self.EXAMPLE_INPUT, 2, "increasing")
        expected = 206
        self.assertEqual(actual, expected)

    def test_determine_increasing_fuel_cost_to_alignment_at_5(self):
        actual = determine_fuel_cost_to_alignment_at(self.EXAMPLE_INPUT, 5, "increasing")
        expected = 168
        self.assertEqual(actual, expected)

    def test_determine_increasing_cheapest_alignment(self):
        actual = determine_cheapest_alignment(self.EXAMPLE_INPUT, "increasing")
        expected = 5
        self.assertEqual(actual, expected)

    def test_determine_increasing_minimum_fuel_cost(self):
        actual = determine_minimum_fuel_cost(self.EXAMPLE_INPUT, "increasing")
        expected = 168
        self.assertEqual(actual, expected)

if __name__ == '__main__':
    unittest.main()

