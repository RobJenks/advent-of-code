from unittest import TestCase
from typing import List, Tuple
from itertools import count
import hashlib

def input():
    return 'ahsbgdzn'

def run():
    print("Part 1 result:", part1(input()))


def part1(input):
    return get_keys(input, 64)[-1]


def get_keys(input, n) -> List[int]:    # [ indices ]
    index, res = 0, []
    pending: List[Tuple[bool, int, str]] = []     # [{ active, index, char[5] }]

    for index in count():
        hash = get_hash(input, index)

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


class Tests(TestCase):
    def test_key_generation(self):
        keys = get_keys("abc", 64)
        print(keys)

        self.assertEqual(39, keys[0])
        self.assertEqual(92, keys[1])
        self.assertEqual(22728, keys[63])
