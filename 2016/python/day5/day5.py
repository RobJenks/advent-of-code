from itertools import islice, count
import hashlib

def input():
    return "ugkcyxxp"

def run():
    print("Part 1 result:", part1(input()))
    print("Part 2 result:", part2(input()))


def part1(input):
    return ''.join(x[0] for x in islice(get_char(input), 8))


def part2(input):
    gen = get_char(input)
    pwd = [' ' for _ in range(8)]

    while True:
        ch = next(gen)
        if ch[0].isdigit() and int(ch[0]) < 8 and pwd[int(ch[0])] == ' ':
            pwd[int(ch[0])] = ch[1]
            if ' ' not in pwd:
                return ''.join(pwd)


def get_char(door_id):
    for index in count():
        digest = hashlib.md5("{}{}".format(door_id, index).encode()).hexdigest()

        if digest[:5] == '00000':
            yield digest[5:7]
