import unittest
from code import *

class TestDay11(unittest.TestCase):
    SMALL_EXAMPLE_INPUT = [
        "11111",
        "19991",
        "19191",
        "19991",
        "11111",
    ]

    EXAMPLE_INPUT = [
        "5483143223",
        "2745854711",
        "5264556173",
        "6141336146",
        "6357385478",
        "4167524645",
        "2176841721",
        "6882881134",
        "4846848554",
        "5283751526",
    ]

    def test_step_small(self):
        actual = step(self.SMALL_EXAMPLE_INPUT)
        expected = [
            "34543",
            "40004",
            "50005",
            "40004",
            "34543",
        ]
        self.assertEqual(actual, expected)
        actual_2 = step(expected)
        expected_2 = [
            "45654",
            "51115",
            "61116",
            "51115",
            "45654",
        ]
        self.assertEqual(actual_2, expected_2)

    def test_step_big(self):
        actual = step(self.EXAMPLE_INPUT)
        expected = [
            "6594254334",
            "3856965822",
            "6375667284",
            "7252447257",
            "7468496589",
            "5278635756",
            "3287952832",
            "7993992245",
            "5957959665",
            "6394862637",
        ]
        self.assertEqual(actual, expected)
        actual_2 = step(expected)
        expected_2 = [
            "8807476555",
            "5089087054",
            "8597889608",
            "8485769600",
            "8700908800",
            "6600088989",
            "6800005943",
            "0000007456",
            "9000000876",
            "8700006848",
        ]
        self.assertEqual(actual_2, expected_2)

    def test_count_flashes(self):
        actual = count_flashes(self.EXAMPLE_INPUT, 100)
        expected = 1656
        self.assertEqual(actual, expected)

    def test_find_simultaneous_flashes(self):
        actual = find_simultaneous_flashes(self.EXAMPLE_INPUT)
        expected = 195
        self.assertEqual(actual, expected)

if __name__ == '__main__':
    unittest.main()

