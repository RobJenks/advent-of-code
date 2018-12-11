#include "day1.h"
#include <iostream>
#include <numeric>
#include <algorithm>

void Day1::Run(void) const
{
    std::vector<std::string> input = GetLines(ReadInput(fs::path("day1/input.txt")));

    std::cout << "Result = " << std::accumulate(input.begin(), input.end(), 0,
        [](int acc, const std::string & el) { return (acc + std::atoi(el.c_str())); }) << "\n";
}