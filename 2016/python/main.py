from day1 import day1
from day2 import day2
from day3 import day3
from day4 import day4
from day5 import day5


def main():
    solutions = [day1.run, day2.run, day3.run, day4.run, day5.run]

    for (i, soln) in enumerate(solutions):
        print(f"\nDay {i+1}:\n")
        soln()


# Entry point
if __name__ == "__main__":
    main()
