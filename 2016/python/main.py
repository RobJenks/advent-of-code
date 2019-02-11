from day1 import day1
from day2 import day2
from day3 import day3
from day4 import day4
from day5 import day5
from day6 import day6
from day7 import day7
from day8 import day8
from day9 import day9
from day10 import day10
from day11 import day11
from day12 import day12


def main():
    solutions = [
        day1.run, day2.run, day3.run, day4.run, day5.run,
        day6.run, day7.run, day8.run, day9.run, day10.run,
        day11.run, day12.run
    ]

    for (i, soln) in enumerate(solutions):
        print(f"\nDay {i+1}:\n")
        soln()


# Entry point
if __name__ == "__main__":
    main()
