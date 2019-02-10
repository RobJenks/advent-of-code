import re
from collections import Counter
from common import io

def run():
    print("Part 1 result:", part1())
    print("Part 2 result:", part2())


def part1():
    return sum(int(x[1]) for x in parse_input("day4/input.txt") if checksum(x[0]) == x[2])


def part2():
    real = list((x[0], int(x[1])) for x in parse_input("day4/input.txt") if checksum(x[0]) == x[2])
    return [x[1] for x in real if 'northpole' in decrypt(*x)]


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


def decrypt(name, id):
    return ''.join([cycle_char(x, id) for x in name])


def cycle_char(ch, id):
    return chr((((ord(ch) - ord('a')) + id) % 26) + ord('a')) if ch != '-' else ' '
