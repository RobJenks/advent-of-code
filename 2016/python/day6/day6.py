import unittest
from collections import Counter
from common import io

def run():
    print("Part 1 result:", part1())
    print("Part 2 result:", part2())


def part1():
    return ''.join([Counter(x).most_common()[0][0]
                    for x in zip(*io.read_file("day6/input.txt").splitlines(False))])


def part2():
    return ''.join([Counter(x).most_common()[-1][0]
                    for x in zip(*io.read_file("day6/input.txt").splitlines(False))])



class Tests(unittest.TestCase):
    def test_freq(self):
        self.assertEqual(''.join([Counter(x).most_common()[0][0]
                                  for x in zip(*io.read_file("tests.txt").splitlines(False))]),
                         "easter")

    def test_least_freq(self):
        self.assertEqual(''.join([Counter(x).most_common()[-1][0]
                                  for x in zip(*io.read_file("tests.txt").splitlines(False))]),
                         "advent")
