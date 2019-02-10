from unittest import TestCase
from common import io

def run():
    print("Part 1 result:", part1())
    print("Part 2 result:", part2())


def part1():
    return len(decompress(io.read_file("day9/input.txt")).strip())


def part2():
    return recursive_decompress(io.read_file("day9/input.txt"))


def decompress(input):
    i, op, n = 0, -1, len(input)
    out = []
    while i < n:
        if input[i] == '(':
            op = i
        elif input[i] == ')':
            val = [int(x) for x in input[op+1:i].split('x')]
            out.extend(input[i+1 : i+1+val[0]] * val[1])
            i += val[0]
            op = -1
        else:
            if op == -1: out.append(input[i])

        i += 1
    return ''.join(out)


def recursive_decompress(input):
    i, op, n = 0, -1, len(input)
    length = 0
    while i < n:
        if input[i] == '(':
            op = i
        elif input[i] == ')':
            val = [int(x) for x in input[op+1:i].split('x')]
            length += (val[1] * recursive_decompress(input[i+1:i+1+val[0]]))
            i += val[0]
            op = -1
        else:
            if op == -1: length += 1

        i += 1

    return length


class Tests(TestCase):
    def test_decompress(self):
        self.assertEqual("ADVENT", decompress("ADVENT"))
        self.assertEqual("ABBBBBC", decompress("A(1x5)BC"))
        self.assertEqual("XYZXYZXYZ", decompress("(3x3)XYZ"))
        self.assertEqual("ABCBCDEFEFG", decompress("A(2x2)BCD(2x2)EFG"))
        self.assertEqual("(1x3)A", decompress("(6x1)(1x3)A"))
        self.assertEqual("X(3x3)ABC(3x3)ABCY", decompress("X(8x2)(3x3)ABCY"))

    def test_full_decompress(self):
        self.assertEqual(9, recursive_decompress("(3x3)XYZ"))
        self.assertEqual(20, recursive_decompress("X(8x2)(3x3)ABCY"))
        self.assertEqual(241920, recursive_decompress("(27x12)(20x12)(13x14)(7x10)(1x12)A"))
        self.assertEqual(445, recursive_decompress("(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN"))
