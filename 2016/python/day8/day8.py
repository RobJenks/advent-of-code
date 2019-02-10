import unittest
from enum import IntEnum
from common import io, util

def run():
    print("Part 1 result:", part1())


def part1():
    return sum(sum(1 for x in row if x) for row in process_commands(parse_input("day8/input.txt"), (50, 6)))


def process_commands(cmds, size):
    d = [[False] * size[0] for _ in range(size[1])]
    for x in cmds:

        if x[0] == CommandType.Rect:
            for row in range(x[2]):
                d[row][:x[1]] = [True] * x[1]

        elif x[0] == CommandType.RotateRow:
            d[x[1]] = util.rotate(d[x[1]], -x[2])

        elif x[0] == CommandType.RotateCol:
            vals = [r[x[1]] for r in d]
            vals = util.rotate(vals, -x[2])
            for (i, v) in enumerate(vals):
                d[i][x[1]] = v

    return d


def parse_input(path):
    res, rows = [], [x.split() for x in io.read_file(path).splitlines()]
    for x in rows:
        res.append(
            (CommandType.Rect, int(x[1].split('x')[0]), int(x[1].split('x')[1])) if x[0] == 'rect'
            else (CommandType.RotateRow, int(x[2].split('=')[1]), int(x[4])) if x[1] == 'row'
            else (CommandType.RotateCol, int(x[2].split('=')[1]), int(x[4])) if x[1] == 'column'
            else 'ERROR')

    return res


class CommandType(IntEnum):
    Rect = 0
    RotateRow = 1
    RotateCol = 2



class Tests(unittest.TestCase):
    def test_commands(self):
        d = process_commands(parse_input("tests.txt"), (7, 3))
        self.assertEqual(6, sum(sum(1 for x in row if x) for row in d))
