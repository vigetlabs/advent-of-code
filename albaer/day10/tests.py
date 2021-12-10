import unittest
from code import *

class TestDay10(unittest.TestCase):
    EXAMPLE_INPUT = [
      "[({(<(())[]>[[{[]{<()<>>",
      "[(()[<>])]({[<{<<[]>>(",
      "{([(<{}[<>[]}>{[]{[(<()>",
      "(((({<>}<{<{<>}{[]{[]{}",
      "[[<[([]))<([[{}[[()]]]",
      "[{[{({}]{}}([{[{{{}}([]",
      "{<[[]]>}<{[{[{[]{()[[[]",
      "[<(<(<(<{}))><([]([]()",
      "<{([([[(<>()){}]>(<<{{",
      "<{([{{}}[<[[[<>{}]]]>[]]",
    ]
    EXAMPLE_CORRUPT_BRACE = "{([(<{}[<>[]}>{[]{[(<()>" # Expected ], but found } instead.
    EXAMPLE_CORRUPT_PAREN = "[[<[([]))<([[{}[[()]]]" # Expected ], but found ) instead.
    EXAMPLE_CORRUPT_BRACK = "[{[{({}]{}}([{[{{{}}([]" # Expected ), but found ] instead.
    EXAMPLE_CORRUPT_PAREN_2 = "[<(<(<(<{}))><([]([]()" # Expected >, but found ) instead.
    EXAMPLE_CORRUPT_ARROW = "<{([([[(<>()){}]>(<<{{" # Expected ], but found > instea

    # Part 1

    def test_calculate_points_0(self):
        actual = calculate_points(self.EXAMPLE_CORRUPT_BRACE)
        expected = 1197
        self.assertEqual(actual, expected)

    def test_calculate_points_1(self):
        actual = calculate_points(self.EXAMPLE_CORRUPT_PAREN)
        expected = 3
        self.assertEqual(actual, expected)

    def test_calculate_points_2(self):
        actual = calculate_points(self.EXAMPLE_CORRUPT_BRACK)
        expected = 57
        self.assertEqual(actual, expected)

    def test_calculate_points_3(self):
        actual = calculate_points(self.EXAMPLE_CORRUPT_PAREN_2)
        expected = 3
        self.assertEqual(actual, expected)

    def test_calculate_points_4(self):
        actual = calculate_points(self.EXAMPLE_CORRUPT_ARROW)
        expected = 25137
        self.assertEqual(actual, expected)

    def test_total_points(self):
        actual = total_points(self.EXAMPLE_INPUT)
        expected = 26397
        self.assertEqual(actual, expected)


if __name__ == '__main__':
    unittest.main()

