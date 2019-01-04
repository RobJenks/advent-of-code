#include "day17.h"
#include <iostream>
#include <cassert>
#include "../common/StringUtil.h"
#include "../common/Test.h"


void Day17::Run(void) const
{
    std::cout << "\nDay 17:\n";

    RunTests();
    Part1();
}

void Day17::RunTests(void) const
{
    std::vector<std::string> input = GetLines(ReadInput("day17/tests.txt"));

    auto terrain_input = ParseTerrainInput(input);
    Terrain terrain(terrain_input, { 500,0 });
    
    terrain.EvaluateToEquilibrium();
    std::cout << "\n" << terrain.str() << "\n";

    auto result = terrain.GetWaterCellCount();
    Test::AssertVerbose(result, 57, "Incorrect water cell count", "Water cells");
}

void Day17::Part1(void) const
{
    std::vector<std::string> input = GetLines(ReadInput("day17/input.txt"));

    auto terrain_input = ParseTerrainInput(input);
    Terrain terrain(terrain_input, { 500,0 });
    
    terrain.EvaluateToEquilibrium();
    std::cout << "\n" << terrain.str() << "\n";

    auto result = terrain.GetWaterCellCount();
    std::cout << "Part 1 result: " << result << "\n";
}

Terrain::TerrainInputCollection Day17::ParseTerrainInput(const std::vector<std::string> & input) const
{
    char c0, c1; int v0, v1a, v1b;
    Terrain::TerrainInputCollection terrain;

    for (auto line : input)
    {
        StringUtil::TrimInPlace(line);
        if (line.empty()) continue;

        sscanf_s(line.c_str(), "%c=%d, %c=%d..%d", &c0, 1, &v0, &c1, 1, &v1a, &v1b);
        if (c0 == 'x')
        {
            terrain.push_back({ {v0,v0}, {v1a,v1b} });  // {x, {y..y}}
        }
        else if (c0 == 'y')
        {
            terrain.push_back({ {v1a,v1b}, {v0,v0} });  // {{x..x}, y}
        }
        else
        {
            std::cout << "ERROR: Unexpected input coord '" << c0 << "'\n";
            assert(false);
        }
    }

    return terrain;
}