from unittest import TestCase
from enum import IntEnum, auto
from common import io, util

def run():
    print("Part 1 result:", part1())


def part1():
    return process('abcdefgh', parse_input(io.read_file("day21/input.txt")))


def process(value, commands):
    x = [x for x in value]
    for cmd in commands:
        if cmd[0] == Command.SwapPosition:
            swap_position(x, cmd[1], cmd[2])
        elif cmd[0] == Command.SwapLetter:
            swap_position(x, x.index(cmd[1]), x.index(cmd[2]))
        elif cmd[0] == Command.Rotate:
            x = util.rotate(x, cmd[1])
        elif cmd[0] == Command.RotateByLetterPos:
            x = util.rotate(x, -(1 + x.index(cmd[1]) + (1 if x.index(cmd[1]) >= 4 else 0)))
        elif cmd[0] == Command.Reverse:
            x[cmd[1]:cmd[2]+1] = x[cmd[1]:cmd[2]+1][::-1]
        elif cmd[0] == Command.Move:
            v = x.pop(cmd[1])
            x.insert(cmd[2], v)

    return ''.join(x)


def swap_position(x, i0, i1):
    x[i0], x[i1] = x[i1], x[i0]


def parse_input(input):
    res = []
    for x in [x.split() for x in input.splitlines(False)]:
        if x[0:2] == ['swap', 'letter']:
            res.append((Command.SwapLetter, x[2], x[5]))
        elif x[0:2] == ['swap', 'position']:
            res.append((Command.SwapPosition, int(x[2]), int(x[5])))
        elif x[0:2] == ['rotate', 'based']:
            res.append((Command.RotateByLetterPos, x[6]))
        elif x[0] == 'rotate':
            res.append((Command.Rotate, int(x[2]) * (-1 if x[1] == 'right' else 1)))
        elif x[0] == 'reverse':
            res.append((Command.Reverse, int(x[2]), int(x[4])))
        elif x[0] == 'move':
            res.append((Command.Move, int(x[2]), int(x[5])))
        else:
            raise RuntimeError("Invalid command: {}".format(x))

    return res


class Command(IntEnum):
    SwapPosition = auto()
    SwapLetter = auto()
    Rotate = auto()
    RotateByLetterPos = auto()
    Reverse = auto()
    Move = auto()


class Tests(TestCase):
    def test_commands(self):
        self.assertEqual('decab', process('abcde', parse_input(io.read_file("tests.txt"))))
