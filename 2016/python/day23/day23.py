from unittest import TestCase
from functools import reduce
from operator import mul
from math import factorial
from day12 import day12

def run():
    print("Part 1 result:", part1())
    print("Part 2 result:", part2())


def part1():
    (reg_final, _) = day12.execute(day12.parse_input("day23/input.txt"), [7, 0, 0, 0])
    return reg_final[0]


def part2():
    inst = day12.parse_input("day23/input.txt")
    return reduce(mul, [inst[i].args[0] for i in range(19, 21)], 1) + factorial(12)


class Tests(TestCase):
    def test_execution(self):
        (reg_final, _) = day12.execute(day12.parse_input("tests.txt"), [0, 0, 0, 0])
        self.assertEqual(3, reg_final[0])
