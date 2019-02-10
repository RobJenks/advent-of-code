from math import fmod
import unittest


def clamp(x, l, u):
    return l if (x < l) else u if (x > u) else x


def rotate(lst, n):
    n = int(fmod(n, len(lst)))
    return lst[n:] + lst[:n]



class Tests(unittest.TestCase):
    def test_rotate(self):
        self.assertEqual([1, 2, 3, 4, 5], rotate([1, 2, 3, 4, 5], 0))
        self.assertEqual([4, 5, 1, 2, 3], rotate([1, 2, 3, 4, 5], 3))
        self.assertEqual([4, 5, 1, 2, 3], rotate([1, 2, 3, 4, 5], 3 + 5 + 5 + 5))
        self.assertEqual([2, 3, 4, 5, 1], rotate([1, 2, 3, 4, 5], -4))
        self.assertEqual([2, 3, 4, 5, 1], rotate([1, 2, 3, 4, 5], -14))
