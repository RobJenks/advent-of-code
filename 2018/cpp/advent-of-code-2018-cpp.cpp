#include <iostream>
#include <vector>
#include "base/AOCSolution.h"
#include "day1/day1.h"
#include "day2/day2.h"

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
    Run(Day1(), Day2());
}

