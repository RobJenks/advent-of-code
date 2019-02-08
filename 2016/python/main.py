from day1 import day1
from day2 import day2


def main():
    solutions = [day1.run, day2.run]

    for (i, soln) in enumerate(solutions):
        print(f"\nDay {i+1}:\n")
        soln()



if __name__ == "__main__":
    main()
