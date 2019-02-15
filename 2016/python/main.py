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
from day13 import day13
from day14 import day14
from day15 import day15
from day16 import day16
from day17 import day17
from day18 import day18
from day19 import day19
from day20 import day20
from day21 import day21


def main():
    solutions = [
        day1.run, day2.run, day3.run, day4.run, day5.run,
        day6.run, day7.run, day8.run, day9.run, day10.run,
        day11.run, day12.run, day13.run, day14.run, day15.run,
        day16.run, day17.run, day18.run, day19.run, day20.run,
        day21.run
    ]

    for (i, soln) in enumerate(solutions):
        print(f"\nDay {i+1}:\n")
        soln()


# Entry point
if __name__ == "__main__":
    main()
