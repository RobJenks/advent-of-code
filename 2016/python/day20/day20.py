from unittest import TestCase
from common import io

def run():
    print("Part 1 result:", part1())


def part1():
    return first_valid(parse_input(io.read_file("day20/input.txt")))


def first_valid(ranges):
    max_val = 0
    for r in [[0, 0], *sorted(ranges)]:
        if r[0] > (max_val + 1):
            return max_val + 1
        max_val = max(max_val, r[1])

    raise RuntimeError("Failed to find valid address")


def parse_input(input):
    return [[int(y) for y in x.split('-')] for x in input.splitlines(False)]


class Tests(TestCase):
    def test_first_valid(self):
        self.assertEqual(3, first_valid(parse_input("5-8\n0-2\n4-7")))
