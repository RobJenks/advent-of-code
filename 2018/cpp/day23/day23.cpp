#include "day23.h"
#include <iostream>
#include "../common/Test.h"
#include "../common/Vec3.h"


void Day23::Run(void) const
{
    std::cout << "\nDay 23:\n";

    RunTests();
    Part1();
}


void Day23::RunTests(void) const
{
    
}

void Day23::Part1(void) const
{
    auto input = GetLines(ReadInput("day23/input.txt"));
    auto bots = ParseInput(input);

    auto strongest = bots.GetStrongest();
    auto in_range = bots.GetBotsInRange(strongest);

    std::cout << "Part 1 result: " << in_range << " (of " << bots.Count() << " bots in range of leader " << bots.Get(strongest) << ")\n";
}


Bots Day23::ParseInput(const std::vector<std::string> & input) const
{
    Bots bots;
    Vec3<long> pos; long radius;

    for (const std::string & line : input)
    {
        sscanf_s(line.c_str(), "pos=<%ld,%ld,%ld>, r=%ld", &pos.x, &pos.y, &pos.z, &radius);
        bots.Add(pos, radius);
    }

    return bots;
}