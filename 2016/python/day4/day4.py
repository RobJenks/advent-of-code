import re
from collections import Counter
from common import io

def run():
    print("Part 1 result:", part1())


def part1():
    return sum(int(x[1]) for x in parse_input("day4/input.txt") if checksum(x[0]) == x[2])


def parse_input(path):
    return [extract_input(x) for x in io.read_file(path).splitlines(False)]


def extract_input(s):
    match = re.search(r'([a-z\-]*)-([0-9]*)\[([a-z]*)\]', s)
    if not match:
        raise RuntimeError("No input match")

    return tuple(match.group(i) for i in range(1, 4))


def checksum(name):
    c = list(Counter([x for x in name if x != '-']).items())
    c = sorted(c, key=lambda x: x[0], reverse=False)
    return ''.join([x[0] for x in sorted(c, key=lambda x: x[1], reverse=True)[:5]])