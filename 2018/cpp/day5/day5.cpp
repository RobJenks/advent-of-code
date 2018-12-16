#include "day5.h"
#include <iostream>
#include <array>
#include <numeric>


void Day5::Run(void) const
{
    std::cout << "\nDay 5:\n";

    Part1();
    Part2();
}

void Day5::Part1(void) const
{
    std::string input = ReadInput(fs::path("day5/input.txt"));

    std::string reduced = Reduce(input);
    std::string result = Collapse(reduced, [](char c) { return (c != REMOVED); });

    std::cout << "Part 1 result = " << result.size() << " units\n";
}

void Day5::Part2(void) const
{
    std::cout << "Calculating part 2: ";
    std::string input = ReadInput(fs::path("day5/input.txt"));

    std::array<size_t, 26U> results = { 0 };
    for (char i = 0; i < 26; ++i)
    {
        // Collapse result first to remove target symbols
        std::cout << ".";
        std::string collapsed = Collapse(input, [i](char c) { return (c != ('a' + i) && c != ('A' + i)); });

        // Now reduce/collapse as normal and record the length of the final symbol string
        std::string result = Collapse(Reduce(collapsed), [](char c) { return (c != REMOVED); });
        results[i] = result.size();
    }

    size_t result = std::accumulate(results.begin(), results.end(), input.size(), 
        [](size_t acc, size_t el) { return std::min(acc, el); }
    );

    std::cout << "\nPart 2 result = " << result << "\n";
}


std::string Day5::Reduce(std::string data) const
{
    // Add a pattern-start and -terminator symbols to save comparisons
    data = (PATTERN_START + data + PATTERN_TERM);
    
    // Iterate though all non-removed symbols
    auto current = data.begin(), next = data.begin();
    auto last = (data.end() - 1);
    while (current != last)
    {
        // 'Current' always starts on a non-removed cell.  Advance to find the following symbol
        next = current + 1;
        while (*next == REMOVED) ++next;

        if (will_react(*current, *next))
        {
            // Either eliminate the reacting pair (and move current pointer backwards, to account for cascading effects)
            *current = REMOVED;
            *next = REMOVED;
            while (*current == REMOVED) --current;
        }
        else
        {
            // Or advance the current pointer to current next pointer (which we know is the next non-removed symbol)
            current = next;
        }
    }

    // Remove pattern start/terminator and return
    data.erase(0U, 1U);
    data.pop_back();
    return data;
}


