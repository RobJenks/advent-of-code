from common import io, util


def run():
    print("Part 1 result:", part1())


def part1():
    return [list(reversed(list(get_key(x))))[0] for x in parse_input("day2/input.txt")]


def parse_input(path):
    return [list(x) for x in io.read_file(path).splitlines(False)]


def get_key(path):
    ptr, keys = [1, 1], [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
    for ch in path:
        ptr = [util.clamp(x + y, 0, 2) for (x, y) in zip(ptr, DIRECTIONS[ch])]
        yield keys[ptr[1]][ptr[0]]


DIRECTIONS = {'L': [-1, 0], 'U': [0, -1], 'R': [1, 0], 'D': [0, 1]}
