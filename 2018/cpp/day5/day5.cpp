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

    int cycles = 0;
    while (Reduce(input)) {
        if (++cycles % COLLAPSE_INTERVAL == 0) {
            input = Collapse(input);
        }
    }
    
    std::string result = Collapse(input);
    std::cout << "Part 1 result == " << result << " (" << cycles << " cycles)\n";
}

bool Day5::Reduce(std::string & data) const
{
    // Add a pattern-terminator to save comparisons
    data.push_back(TERMINATOR);
    data.push_back(TERMINATOR);
    
    auto current = data.begin(), next = data.begin();
    bool removed = false;
    
    // Iterate though all non-removed symbols
    auto last = (data.end() - 1);
    while (next != last)
    {
        // Advance to the next symbol
        while (*current == REMOVED) ++current;

        // Advance to find the following symbol
        next = current + 1;
        while (*next == REMOVED) ++next;

        if (will_react(*current, *next))
        {
            // Either eliminate the reacting pair (and stay here; will advance in the next cycle)
            *current = REMOVED;
            *next = REMOVED;
            removed = true;
        }
        else
        {
            // Or advance the current pointer to current next pointer (which we know is the next non-removed symbol)
            current = next;
        }
    }

    // Remove pattern terminator and return
    data.pop_back(); data.pop_back();
    return removed;
}

std::string Day5::Collapse(const std::string & data) const
{
    std::string collapsed;
    std::for_each(data.begin(), data.end(), [&collapsed](const auto & c) { if (c != REMOVED) collapsed.push_back(c); });

    return collapsed;
}

