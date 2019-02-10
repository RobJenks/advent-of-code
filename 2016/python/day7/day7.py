import re
import unittest
from common import io

def run():
    print("Part 1 result:", part1())
    print("Part 2 result:", part2())


def part1():
    return sum(1 for x in io.read_file("day7/input.txt").splitlines(False) if is_tls(x))


def part2():
    return sum(1 for x in io.read_file("day7/input.txt").splitlines(False) if is_ssl(x))


def is_tls(addr):
    found, bracketed = [], False
    for (i, x) in enumerate(addr[:-3]):
        bracketed = True if x == '[' else False if x == ']' else bracketed
        if x == addr[i + 3] and x != addr[i + 1] and addr[i + 1] == addr[i + 2]:
            if bracketed and ']' in addr[i:]: return []
            found.append(addr[i:i + 4])

    return found


def is_ssl(addr):
    aba, bracketed = [], False
    for (i, x) in enumerate(addr[:-2]):
        bracketed = True if x == '[' else False if x == ']' else bracketed
        if not bracketed and x == addr[i + 2] and x != addr[i + 1]:
            aba.append(addr[i:i+3])

    return sum(1 for x in aba if re.search(r"\[[a-z]*?{}[a-z]*?\]".format(re.escape(''.join([x[1], x[0], x[1]]))), addr)) != 0


class Tests(unittest.TestCase):
    def test_tls(self):
        self.assertTrue(is_tls("abba[mnop]qrst"))
        self.assertFalse(is_tls("abcd[bddb]xyyx"))
        self.assertFalse(is_tls("aaaa[qwer]tyui"))
        self.assertTrue(is_tls("ioxxoj[asdfgh]zxcvbn"))

    def test_ssl(self):
        self.assertTrue(is_ssl("aba[bab]xyz"))
        self.assertFalse(is_ssl("xyx[xyx]xyx"))
        self.assertTrue(is_ssl("aaa[kek]eke"))
        self.assertTrue(is_ssl("zazbz[bzb]cdb"))
        self.assertFalse(is_ssl("aba[cd]bab[dc]"))
