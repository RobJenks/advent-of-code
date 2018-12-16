#include "day5.h"
#include <iostream>


void Day5::Run(void) const
{
    std::cout << "\nDay 5:\n";

    Part1();
}

void Day5::Part1(void) const
{
    std::string input = ReadInput(fs::path("day5/input.txt"));

    std::string result = Collapse(Reduce(input));
    std::cout << "Part 1 result = " << result.size() << " units\n";
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

std::string Day5::Collapse(const std::string & data) const
{
    std::string collapsed;
    std::for_each(data.begin(), data.end(), [&collapsed](const auto & c) { if (c != REMOVED) collapsed.push_back(c); });

    return collapsed;
}

