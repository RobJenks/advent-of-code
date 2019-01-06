#include "day18.h"
#include <iostream>
#include <cassert>
#include "../common/Vec2.h"
#include "../common/StringUtil.h"
#include "../common/Test.h"


void Day18::Run(void) const
{
    std::cout << "\nDay 18:\n";

    RunTests();
    Part1();
    Part2();
}

void Day18::RunTests(void) const
{
    std::vector<std::string> input = GetLines(ReadInput("day18/tests.txt"));
 
    std::vector<std::vector<std::string>> steps = 
    {
        { input.begin() + 0, input.begin() + 10 },
        { input.begin() + 11, input.begin() + 21 },
        { input.begin() + 22, input.begin() + 32 },
        { input.begin() + 33, input.begin() + 43 },
        { input.begin() + 44, input.begin() + 54 },
        { input.begin() + 55, input.begin() + 65 },
        { input.begin() + 66, input.begin() + 76 },
        { input.begin() + 77, input.begin() + 87 },
        { input.begin() + 88, input.begin() + 98 },
        { input.begin() + 99, input.begin() + 109 },
        { input.begin() + 110, input.begin() + 120 },
    };

    Forest forest = CreateForest(steps[0]);
    std::cout << "Initial state:\n" << forest.str() << "\n";

    for (auto it = steps.cbegin() + 1; it != steps.cend(); ++it)
    {
        std::cout << "\nStep 1:\n";
        forest.Evaluate();
        Test::AssertComplexVerbose(forest.str(), CreateForest(*it).str());
    }

}

void Day18::Part1(void) const
{
    std::vector<std::string> input = GetLines(ReadInput("day18/input.txt"));
    
    std::cout << "Part 1:\n";
    Forest forest = CreateForest(input);
    
    forest.Evaluate(10);
    std::cout << "\n" << forest.str() << "\n";
    
    auto value = forest.GetResourceValue();
    std::cout << "Part 1 result: " << value << "\n";
}

void Day18::Part2(void) const
{
    std::vector<std::string> input = GetLines(ReadInput("day18/input.txt"));
    Forest forest = CreateForest(input);

    std::vector<Forest::TData> data;        // Collection of forest states at each iteration
    const size_t ITERATIONS = 1000000000U;  // Not expecting to go this far...

    std::cout << "Part 2 calculating\n";
    size_t limit = ITERATIONS;
    for (size_t i = 0; i < limit; ++i)
    {
        forest.Evaluate();
        data.push_back(forest.GetData());

        auto match = std::find(data.begin(), data.end()-1, data.back());
        if (match != data.end()-1)
        {
            auto it0 = std::distance(data.begin(), match);
            auto it1 = std::distance(data.begin(), data.end() - 1);

            auto difference = (ITERATIONS - it0) % (it1 - it0);
            std::cout << "Iteration " << i << ": Found cycle with period " << (it1 - it0) << " from iteration " << it0 << " to " << it1 << "\n";
            std::cout << "(1,000,000,000 - " << it0 << ") % (" << it1 << " - " << (it1 - it0) << ") = " << difference << "\n";
            limit = (it1 + difference);
        }
    }

    std::cout << "Evaluated to iteration " << limit << ", periodically-equivalent to iteration 1,000,000,000\n";
    std::cout << "Part 2 result: " << forest.GetResourceValue() << "\n";
}

Forest Day18::CreateForest(const std::vector<std::string> & input) const
{
    Vec2<int> size(static_cast<int>(StringUtil::Trim(input.at(0)).size()), static_cast<int>(input.size()));
    
    Forest forest(size);
    forest.Populate(input);

    return forest;
}