#include "day1.h"
#include <iostream>
#include <numeric>
#include <algorithm>
#include <unordered_set>

void Day1::Run(void) const
{
    std::cout << "\nDay 1:\n";

    Part1();
    Part2();
}


void Day1::Part1(void) const
{
    std::vector<std::string> input = GetLines(ReadInput(fs::path("day1/input.txt")));

    std::cout << "Part 1 result = " << std::accumulate(input.begin(), input.end(), 0,
        [](int acc, const std::string & el) { return (acc + std::atoi(el.c_str())); }) << "\n";
}

void Day1::Part2(void) const
{
    std::vector<std::string> input = GetLines(ReadInput(fs::path("day1/input.txt")));

    std::vector<int> parsed;
    parsed.reserve(input.size());
    std::transform(input.begin(), input.end(), parsed.begin(), [](const std::string & el) { return std::atoi(el.c_str()); });

    int index = 0, acc = 0;
    std::unordered_set<int> history;

    while (true)
    {
        if (history.find(acc) != history.end()) break;
        history.emplace(acc);

        acc += parsed[index];

        index = ((index + 1) % input.size());
    }

    std::cout << "Part 2 result = " << acc << " (after " << history.size() << " steps)\n";
}