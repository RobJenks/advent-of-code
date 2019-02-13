from unittest import TestCase
from common import io


def run():
    print("Part 1 result:", part1())


def part1():
    return safe_count(generate(parse_input(io.read_file("day18/input.txt")), 40))


def generate(row_0, rows):
    res = [row_0[:]]
    for i in range(rows - 1):
        res.append(generate_row(res[i]))

    return res


def generate_row(prev):
    row = [is_trap([False, prev[0], prev[1]])]
    for i in range(1, len(prev) - 1):
        row.append(is_trap(prev[i-1:i+2]))

    return row + [is_trap([prev[-2], prev[-1], False])]


def safe_count(rows):
    return sum(sum(0 if x else 1 for x in row) for row in rows)


def is_trap(adj):
    return adj == [True, True, False] or \
           adj == [False, True, True] or \
           adj == [True, False, False] or \
           adj == [False, False, True]


def parse_input(input):
    return [x == '^' for x in input]


class Tests(TestCase):
    def test_generation(self):
        self.assertEqual(6, safe_count(generate(parse_input('..^^.'), 3)))
        self.assertEqual(38, safe_count(generate(parse_input('.^^.^.^^^^'), 10)))
