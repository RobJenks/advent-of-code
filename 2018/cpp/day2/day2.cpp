#include "day2.h"
#include <iostream>
#include <numeric>
#include <algorithm>
#include <unordered_set>

void Day2::Run(void) const
{
    std::cout << "\nDay 2:\n";

    Part1();
    Part2();
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

void Day2::Part2(void) const
{
    std::vector<std::string> input = GetLines(ReadInput(fs::path("day2/input.txt")));

    std::string masked_result = PerformMaskedSearch(input);

    auto mask_index = masked_result.find(MASK);
    std::string result = masked_result.substr(0, mask_index) + masked_result.substr(mask_index + 1);

    std::cout << "Part 2 result = " << result << "\n";
}


// Should be O(n * m * m) for n = input size, m = length of a string.  m is constant so -> O(const * n) = O(n) = linear
std::string Day2::PerformMaskedSearch(const std::vector<std::string> & input) const
{
    // Matching transactions will be equal with one character removed, so explode str -> |str| items with masked character
    std::unordered_set<std::string> ids;
    for (const auto & el : input)
    {
        const auto masked = GenerateMaskedIds(el);
        const auto it = std::find_if(masked.begin(), masked.end(), [&ids](const auto & el)
        {
            if (ids.find(el) != ids.end()) return true;
            ids.emplace(el);
            return false;
        });

        if (it != masked.end()) return *it;
    }

    return "No solution";
}



std::array<int, 26U> Day2::ParseID(const std::string id) const
{
    std::array<int, 26U> freq = { 0 };

    std::for_each(id.begin(), id.end(), [&freq](const std::string::value_type c) { ++freq[c - 'a']; });
    return freq;
}

std::vector<std::string> Day2::GenerateMaskedIds(const std::string & id) const
{
    std::vector<std::string> masked;
    for (int i = 0; i < id.size(); ++i)
    {
        std::string masked_id = id;
        masked_id[i] = MASK;
        masked.push_back(masked_id);
    }

    return masked;
}



