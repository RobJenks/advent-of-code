from itertools import islice
import hashlib

def input():
    return "ugkcyxxp"

def run():
    print("Part 1 result:", part1(input()))


def part1(input):
    return ''.join(list(islice(get_char(input), 8)))


def get_char(door_id):
    index = 0
    while True:
        digest = hashlib.md5("{}{}".format(door_id, index).encode()).hexdigest()
        index += 1

        if digest[:5] == '00000':
            yield digest[5:6]
