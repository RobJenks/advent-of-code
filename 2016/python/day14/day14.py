from unittest import TestCase
from typing import List, Tuple
from itertools import count
import hashlib

def input():
    return 'ahsbgdzn'

def run():
    print("Part 1 result:", part1(input()))
    print("Part 2 result:", part2(input()))


def part1(input):
    return get_keys(input, 64, get_hash)[-1]


def part2(input):
    return get_keys(input, 64, get_stretched_hash)[-1]


def get_keys(input, n, generator) -> List[int]:    # [ indices ]
    index, res = 0, []
    pending: List[Tuple[bool, int, str]] = []     # [{ active, index, char[5] }]

    for index in count():
        hash = generator(input, index)

        # Validate any pending hashes, if possible
        threshold = index - 1000
        for i in range(len(pending)):
            (active, ix, pattern) = pending[i]
            if active and ix > threshold and pattern in hash:
                pending[i] = (False, ix, pattern)
                res.append(ix)
                if len(res) == n:
                    return sorted(res)

        # Test whether this is a new candidate
        for i in range(len(hash)-2):
            if hash[i] == hash[i+1] == hash[i+2]:
                pending.append((True, index, hash[i] * 5))
                break


def get_hash(salt, index):
    return hashlib.md5(format("{}{}".format(salt, index)).encode()).hexdigest()


def get_stretched_hash(salt, index):
    x = format("{}{}".format(salt, index))
    for i in range(2017):
        x = hashlib.md5(x.encode()).hexdigest()

    return x


class Tests(TestCase):
    def test_key_generation(self):
        keys = get_keys("abc", 64, get_hash)

        self.assertEqual(39, keys[0])
        self.assertEqual(92, keys[1])
        self.assertEqual(22728, keys[63])

    def test_stretched_hash(self):
        self.assertEqual("a107ff634856bb300138cac6568c0f24", get_stretched_hash("abc", 0))

    def test_stretched_key_generation(self):
        keys = get_keys("abc", 64, get_stretched_hash)
        self.assertEqual(10, keys[0])
        self.assertEqual(22551, keys[63])
