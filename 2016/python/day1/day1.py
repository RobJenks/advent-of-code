import unittest
from enum import IntEnum
from common import io


def run():
    print("Part 1 result:", part1())
    print("Part 2 result:", part2())


def part1():
    return sum(abs(x) for x in final_pos(parse_input(io.read_file("day1/input.txt"))))


def part2():
    visited = set()
    for path in follow_steps(parse_input(io.read_file("day1/input.txt"))):
        steps = {(x, y) for [x, y] in path}
        for match in steps & visited:
            return sum(abs(x) for x in match)

        visited.update(steps)


def follow(instructions):
    pos, direction = [0, 0], Direction.Up

    for inst in instructions:
        direction = ROT[inst[:1]][direction]
        pos = [pos[i] + (x * int(inst[1:])) for (i, x) in enumerate(VEC[direction])]
        yield pos


def follow_steps(instructions):
    pos, direction = (0, 0), Direction.Up

    for inst in instructions:
        direction = ROT[inst[:1]][direction]
        delta = VEC[direction]
        steps = [(x + pos[0], y + pos[1]) for [x, y] in
                 [[delta[0] * i, delta[1] * i] for i in range(1, int(inst[1:]) + 1)]]
        pos = steps[len(steps)-1]
        yield steps


def final_pos(instructions):
    *_, pos = follow(instructions)
    return pos


def parse_input(input: str):
    return input.split(", ")


class Direction(IntEnum):
    Left = 0
    Up = 1
    Right = 2
    Down = 3


# Directions and rotations
DIRS = [Direction.Left, Direction.Up, Direction.Right, Direction.Down]
ROT = {"L": {DIRS[i]: DIRS[(i-1) % 4] for i in range(4)}, "R": {DIRS[i]: DIRS[(i+1) % 4] for i in range(4)}}
VEC = {Direction.Left: (-1, 0), Direction.Up: (0, -1), Direction.Right: (1, 0), Direction.Down: (0, 1)}


class Tests(unittest.TestCase):

    def test_instructions(self):
        self.assertEqual(sum(abs(x) for x in final_pos(parse_input("R2, L3"))), 5)
        self.assertEqual(sum(abs(x) for x in final_pos(parse_input("R2, R2, R2"))), 2)
        self.assertEqual(sum(abs(x) for x in final_pos(parse_input("R5, L5, R5, R3"))), 12)

    def test_distance(self):
        visited = set()
        for path in follow_steps(parse_input("R8, R4, R4, R8")):
            steps = {(x, y) for [x, y] in path}
            for match in steps & visited:
                self.assertEqual(sum(abs(x) for x in match), 4)

            visited.update(steps)


# Optional entry point
if __name__ == "__main__":
    run()
