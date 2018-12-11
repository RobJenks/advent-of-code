#include "day2.h"
#include <iostream>
#include <numeric>
#include <algorithm>

void Day2::Run(void) const
{
    std::cout << "\nDay 2:\n";

    Part1();
}

void Day2::Part1(void) const
{
    std::vector<std::string> input = GetLines(ReadInput(fs::path("day2/input.txt")));

    std::tuple<int, int> results = std::accumulate(input.begin(), input.end(), std::tuple<int, int>{ 0, 0 }, 
        [this](const std::tuple<int, int> & acc, const std::string & el)
    {
        std::array<int, 26U> freq = this->ParseID(el);
        return std::tuple<int, int>
        {
            std::get<0>(acc) + (std::find(freq.begin(), freq.end(), 2) != freq.end() ? 1 : 0),
            std::get<1>(acc) + (std::find(freq.begin(), freq.end(), 3) != freq.end() ? 1 : 0)
        };
    });
    
    std::cout << "Part 1 result = " << (std::get<0>(results) * std::get<1>(results)) << "\n";
}

std::array<int, 26U> Day2::ParseID(const std::string id) const
{
    std::array<int, 26U> freq = { 0 };

    std::for_each(id.begin(), id.end(), [&freq](const std::string::value_type c) { ++freq[c - 'a']; });
    return freq;
}