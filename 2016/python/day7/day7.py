import unittest
from common import io

def run():
    print("Part 1 result:", part1())


def part1():
    return sum(1 for x in io.read_file("day7/input.txt").splitlines(False) if is_valid(x))


def is_valid(addr):
    found, bracketed = [], False
    for (i, x) in enumerate(addr[:-3]):
        bracketed = True if x == '[' else False if x == ']' else bracketed
        if x == addr[i + 3] and x != addr[i + 1] and addr[i + 1] == addr[i + 2]:
            if bracketed and ']' in addr[i:]: return []
            found.append(addr[i:i + 4])

    return found


class Tests(unittest.TestCase):
    def test_tls(self):
        self.assertTrue(is_valid("abba[mnop]qrst"))
        self.assertFalse(is_valid("abcd[bddb]xyyx"))
        self.assertFalse(is_valid("aaaa[qwer]tyui"))
        self.assertTrue(is_valid("ioxxoj[asdfgh]zxcvbn"))
