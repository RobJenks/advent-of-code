from unittest import TestCase
from typing import Tuple, List
from itertools import count
from common import io

def run():
    print("Part 1 result:", part1())
    print("Part 2 result:", part2())


def part1():
    return optimal_time(parse_input("day15/input.txt"))


def part2():
    return optimal_time([*parse_input("day15/input.txt"), (7, 11, 0)])


def optimal_time(input):
    for i in count():
        if all((x[0] + x[2] + i) % x[1] == 0 for x in input):
            return i


def parse_input(path) -> List[Tuple[int, int, int]]:      # [{ ID, PositionCount, StartingPosition }]
    return [(int(x[1][1:]), int(x[3]), int(x[11][:-1]))
            for x in [line.split() for line in io.read_file(path).splitlines(False)]]


class Tests(TestCase):
    def test_time_calc(self):
        self.assertEqual(5, optimal_time(parse_input("tests.txt")))
