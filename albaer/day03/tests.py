import unittest
from code import calculate_gamma

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

    def test_calculate_gamma(self):
        self.assertEqual(calculate_gamma(self.EXAMPLE_INPUT), 22)


if __name__ == '__main__':
    unittest.main()
