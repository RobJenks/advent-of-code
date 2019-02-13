from unittest import TestCase


def input():
    return '01111001100111011'


def run():
    print("Part 1 result:", part1())


def part1():
    return decode(checksum(dragon_curve(encode(input()), 272)))


# Generate a modified dragon curve over binary data, for the specified number of bits
def dragon_curve(seed, n):
    a = seed[:]
    while len(a) < n:
        a += [False] + [not x for x in reversed(a)]

    return a[:n]


def checksum(data):
    x = data[:]
    while len(x) % 2 == 0:
        x = [x[i] == x[i+1] for i in range(len(x)) if i % 2 == 0]

    return x


def encode(data):
    return [(x == '1') for x in data]


def decode(data):
    return ''.join(['1' if x else '0' for x in data])


class Tests(TestCase):
    def test_curve(self):
        self.assertEqual('100', decode(dragon_curve(encode('1'), 3)))
        self.assertEqual('001', decode(dragon_curve(encode('0'), 3)))
        self.assertEqual('11111000000', decode(dragon_curve(encode('11111'), 11)))
        self.assertEqual('1111000010100101011110000', decode(dragon_curve(encode('111100001010'), 25)))

    def test_checksum(self):
        self.assertEqual('100', decode(checksum(encode('110010110100'))))

    def test_process(self):
        self.assertEqual('01100', decode(checksum(dragon_curve(encode('10000'), 20))))
