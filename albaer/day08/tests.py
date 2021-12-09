import unittest
from code import *

class TestDay08(unittest.TestCase):
    EXAMPLE_INPUT = [
        "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe",
        "edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc",
        "fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg",
        "fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb",
        "aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea",
        "fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb",
        "dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe",
        "bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef",
        "egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb",
        "gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce",
    ]

    EXAMPLE_ENTRY = "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"

    # Part 1

    def test_count_1_4_7_8(self):
        actual = count_1_4_7_8(self.EXAMPLE_INPUT)
        expected = 26
        self.assertEqual(actual, expected)

    # Part 2

    def test_id_patterns(self):
        actual = id_patterns(self.EXAMPLE_ENTRY)
        expected = {
            'cagedb': '0',
            'ab': '1',
            'gcdfa': '2',
            'fbcad': '3',
            'eafb': '4',
            'cdfbe': '5',
            'cdfgeb': '6',
            'dab': '7',
            'acedgfb': '8',
            'cefabd': '9',
        }
        self.assertEqual(actual, expected)

    def test_read_output(self):
        actual = read_output(self.EXAMPLE_ENTRY)
        expected = 5353
        self.assertEqual(actual, expected)

    # def test_sum_output_values(self):
    #     actual = sum_output_values(self.EXAMPLE_INPUT)
    #     expected = 61229
    #     self.assertEqual(actual, expected)

if __name__ == '__main__':
    unittest.main()

