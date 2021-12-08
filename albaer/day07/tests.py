import unittest
from code import *

class TestDay07(unittest.TestCase):
    EXAMPLE_INPUT = [16,1,2,0,4,2,7,1,2,14,]

    # Part 1

    def test_determine_constant_fuel_cost_to_alignment_at_1(self):
        actual = determine_fuel_cost_to_alignment_at(self.EXAMPLE_INPUT, 1, calc_constant_fuel_cost)
        expected = 41
        self.assertEqual(actual, expected)

    def test_determine_constant_fuel_cost_to_alignment_at_2(self):
        actual = determine_fuel_cost_to_alignment_at(self.EXAMPLE_INPUT, 2, calc_constant_fuel_cost)
        expected = 37
        self.assertEqual(actual, expected)

    def test_determine_constant_fuel_cost_to_alignment_at_3(self):
        actual = determine_fuel_cost_to_alignment_at(self.EXAMPLE_INPUT, 3, calc_constant_fuel_cost)
        expected = 39
        self.assertEqual(actual, expected)

    def test_determine_constant_fuel_cost_to_alignment_at_10(self):
        actual = determine_fuel_cost_to_alignment_at(self.EXAMPLE_INPUT, 10, calc_constant_fuel_cost)
        expected = 71
        self.assertEqual(actual, expected)

    def test_determine_constant_minimum_fuel_cost(self):
        actual = determine_minimum_fuel_cost(self.EXAMPLE_INPUT, calc_constant_fuel_cost)
        expected = 37
        self.assertEqual(actual, expected)

    # Part 2

    def test_determine_increasing_fuel_cost_to_alignment_at_2(self):
        actual = determine_fuel_cost_to_alignment_at(self.EXAMPLE_INPUT, 2, calc_increasing_fuel_cost)
        expected = 206
        self.assertEqual(actual, expected)

    def test_determine_increasing_fuel_cost_to_alignment_at_5(self):
        actual = determine_fuel_cost_to_alignment_at(self.EXAMPLE_INPUT, 5, calc_increasing_fuel_cost)
        expected = 168
        self.assertEqual(actual, expected)

    def test_determine_increasing_minimum_fuel_cost(self):
        actual = determine_minimum_fuel_cost(self.EXAMPLE_INPUT, calc_increasing_fuel_cost)
        expected = 168
        self.assertEqual(actual, expected)

if __name__ == '__main__':
    unittest.main()

