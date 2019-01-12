#include <iostream>
#include <vector>
#include "base/AOCSolution.h"
#include "day1/day1.h"
#include "day2/day2.h"
#include "day3/day3.h"
#include "day4/day4.h"
#include "day5/day5.h"
#include "day6/day6.h"
#include "day7/day7.h"
#include "day8/day8.h"
#include "day9/day9.h"
#include "day10/day10.h"
#include "day11/day11.h"
#include "day12/day12.h"
#include "day13/day13.h"
#include "day14/day14.h"
#include "day15/day15.h"
#include "day16/day16.h"
#include "day17/day17.h"
#include "day18/day18.h"
#include "day19/day19.h"
#include "day20/day20.h"
#include "day21/day21.h"
#include "day22/day22.h"
#include "day23/day23.h"


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
    Run(Day23()); return 0;
    Run(Day1(), Day2(), Day3(), Day4(), Day5(), 
        Day6(), Day7(), Day8(), Day9(), Day10(), 
        Day11(), Day12(), Day13(), Day14(), Day15(),
        Day16(), Day17(), Day18(), Day19(), Day20(),
        Day21(), Day22(), Day23());
}

