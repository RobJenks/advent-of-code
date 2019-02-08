import sys
import os.path


def read_file(path):
    if not sys.path[0]:
        return sys.stdin.read()

    with open(os.path.join(sys.path[0], path), 'r') as file:
        return file.read()

