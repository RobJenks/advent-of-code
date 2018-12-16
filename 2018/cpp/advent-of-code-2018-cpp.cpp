#include <iostream>
#include <vector>
#include "base/AOCSolution.h"
#include "day1/day1.h"
#include "day2/day2.h"
#include "day3/day3.h"
#include "day4/day4.h"
#include "day5/day5.h"
#include "day6/day6.h"


template <typename... TSolutions>
void Run(const TSolutions&... solutions)
{
    (RunSolution(solutions), ...);
}

template <typename TSolution>
void RunSolution(const TSolution & solution)
{
    solution.Run();
}

int main(int argc, char *argv[])
{
    Run(Day1(), Day2(), Day3(), Day4(), Day5(), Day6());
}

