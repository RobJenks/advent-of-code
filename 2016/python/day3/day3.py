from operator import concat
from functools import reduce
from common import io

def run():
    print("Part 1 result:", part1())
    print("Part 2 result:", part2())


def part1():
    return sum(1 for x in parse_input("day3/input.txt") if sum(sorted(x)[:2]) > sorted(x)[-1])


def part2():
    data = parse_input("day3/input.txt")
    transposed = reduce(concat, [list(zip(*data[i:i+3])) for i in range(0, len(data), 3)])

    return sum(1 for x in transposed if sum(sorted(x)[:2]) > sorted(x)[-1])


def parse_input(path):
    return [list(map(int, x)) for x in [line.split() for line in io.read_file(path).splitlines()]]
