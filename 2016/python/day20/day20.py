from unittest import TestCase
from common import io

def run():
    print("Part 1 result:", part1())
    print("Part 2 result:", part2())


def part1():
    return first_valid(parse_input(io.read_file("day20/input.txt")))


def part2():
    return valid_count(parse_input(io.read_file("day20/input.txt")), 4294967295)


def first_valid(ranges):
    max_val = 0
    for r in [[0, 0], *sorted(ranges)]:
        if r[0] > (max_val + 1):
            return max_val + 1
        max_val = max(max_val, r[1])

    raise RuntimeError("Failed to find valid address")


def valid_count(ranges, max_permitted):
    max_val, count = 0, 0
    for r in [[0, 0], *sorted(ranges)]:
        if r[0] > (max_val + 1):
            count += (r[0] - (max_val + 1))
        max_val = max(max_val, r[1])

    return count + (max_permitted - max_val)


def parse_input(input):
    return [[int(y) for y in x.split('-')] for x in input.splitlines(False)]


class Tests(TestCase):
    def test_first_valid(self):
        self.assertEqual(3, first_valid(parse_input("5-8\n0-2\n4-7")))

    def test_valid_count(self):
        self.assertEqual(2, valid_count(parse_input("5-8\n0-2\n4-7"), 9))
