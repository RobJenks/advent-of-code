#include "day22.h"
#include <iostream>
#include <cassert>
#include "../common/Test.h"
#include "Cave.h"


void Day22::Run(void) const
{
    std::cout << "\nDay 22:\n";

    RunTests();
    Part1();
}


void Day22::RunTests(void) const
{
    auto input = GetLines(ReadInput("day22/tests.txt"));
    auto data = ParseInput(input);

    Cave cave(data.first, data.second);
    std::cout << "\nTests:\n" << cave.str() << "\n";

    Test::AssertVerbose(cave.CalculateRiskScore(), 114, "Region risk score is incorrect", "Risk score");
}

void Day22::Part1(void) const
{
    auto input = GetLines(ReadInput("day22/input.txt"));
    auto data = ParseInput(input);
    
    Cave cave(data.first, data.second);
    std::cout << "\Part 1:\n" << cave.str() << "\n";

    std::cout << "Part 1 result: " << cave.CalculateRiskScore() << "\n";
}



Day22::InputData Day22::ParseInput(const std::vector<std::string> & input) const
{
    std::string key;
    InputData data = { -1, {-1, -1} };

    for (auto line : input)
    {
        auto cpos = line.find_first_of(',');
        if (cpos != std::string::npos) line[cpos] = ' ';    // To keep parsing simple

        std::stringstream ss(line);
        ss >> key;

        if (key == "depth:") ss >> data.first;
        else if (key == "target:") ss >> data.second.x >> data.second.y;
    }

    assert(data.first != -1 && data.second.x != -1 && data.second.y != -1);
    return data;
}