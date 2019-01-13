#include "day25.h"
#include <iostream>
#include <sstream>
#include "../common/Test.h"


void Day25::Run(void) const
{
    std::cout << "\nDay 25:\n";

    RunTests();
    Part1();
}


void Day25::RunTests(void) const
{
    std::cout << "Testing constellations:\n";
    auto input = GetLines(ReadInput("day25/tests.txt"));

    std::vector<std::pair<std::vector<std::string>, int>> tests =   // { input, expected constellation count }
    {
        { { input.cbegin() + 0, input.cbegin() + 8 }, 2 },
        { { input.cbegin() + 9, input.cbegin() + 19 }, 4 }, 
        { { input.cbegin() + 20, input.cbegin() + 30 }, 3 },
        { { input.cbegin() + 31, input.cbegin() + 41 }, 8 }
    };

    for (const auto & test : tests)
    {
        Points points = ParseInput(test.first);
        Test::AssertVerbose(points.GetConstellationCount(), test.second);
    }
}

void Day25::Part1(void) const
{
    std::cout << "\nPart 1:\n";

    auto input = GetLines(ReadInput("day25/input.txt"));
    auto points = ParseInput(input);

    std::cout << "Part 1 result: " << points.GetConstellationCount() << "\n";
}


Points Day25::ParseInput(const std::vector<std::string> & input) const
{
    std::vector<Vec4<int>> points;

    Vec4<int> p; size_t index;
    for (auto line : input)
    {
        while ((index = line.find(',')) != std::string::npos) line[index] = ' ';

        std::stringstream ss(line);
        ss >> p.x >> p.y >> p.z >> p.w;

        points.push_back(p);
    }

    return Points(points);
}