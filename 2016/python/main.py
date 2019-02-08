from day1 import day1


def main():
    solutions = [day1.run]

    for (i, soln) in enumerate(solutions):
        print(f"\nDay {i+1}:\n")
        soln()



if __name__ == "__main__":
    main()
