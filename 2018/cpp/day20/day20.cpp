#include "day20.h"
#include <iostream>
#include <cassert>
#include "../common/StringUtil.h"
#include "../common/Test.h"
#include "Rooms.h"


void Day20::Run(void) const
{
    std::cout << "\nDay 20:\n";

    RunTests();
    Solve();
}

void Day20::RunTests(void) const
{
    auto input = GetLines(ReadInput("day20/tests.txt"));

    typedef std::tuple<std::string, std::vector<std::string>, int> Scenario;    // { InputPattern, ExpectedMap, ExpectedMaxPath }
    std::vector<Scenario> tests = 
    {
        { "^WNE$",                                                              { input.cbegin() + 0, input.cbegin() + 5 }, 3 },
        { "^ENWWW(NEEE|SSE(EE|N))$",                                            { input.cbegin() + 6, input.cbegin() + 15 }, 10 },
        { "^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$",                          { input.cbegin() + 16, input.cbegin() + 27 }, 18 },
        { "^ESSWWN(E|NNENN(EESS(WNSE|)SSS|WWWSSSSE(SW|NNNE)))$",                { input.cbegin() + 28, input.cbegin() + 41 }, 23 },
        { "^WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))$",  { input.cbegin() + 42, input.cbegin() + 57 }, 31 }
    };

    for (Scenario test : tests)
    {
        auto pattern = std::get<0>(test);
        auto expected_map = StringUtil::Concat(std::get<1>(test));
        auto expected_path = std::get<2>(test);

        std::cout << "\nTest pattern: " << pattern << "\n";

        Rooms rooms(ParseInput(pattern));
        Test::AssertComplexVerbose(rooms.str(), expected_map, "Generated map does not match expected result", "Generated map");
        
        std::cout << "Distance map:\n" << rooms.str_distance() << "\n";
        Test::AssertVerbose(rooms.GetMostDistantRoom(), expected_path, "Path to most distant room not as expected", "Most distant room");
    }
}

void Day20::Solve(void) const
{
    auto input = ParseInput(ReadInput("day20/input.txt"));

    std::cout << "\nPart 1:\n\n";

    Rooms rooms(input);
    std::cout << rooms.str() << "\n";

    std::cout << "Part 1 result: " << rooms.GetMostDistantRoom() << "\n";

    auto distant = std::count_if(rooms.GetDistanceMap().cbegin(), rooms.GetDistanceMap().cend(), [](int el) { return (el >= 1000); });
    std::cout << "Part 2 result: " << distant << " (of " << rooms.GetDistanceMap().size() << " total cells)\n";
}


std::string Day20::ParseInput(std::string input) const
{
    StringUtil::TrimInPlace(input);

    assert(input.front() == '^');
    assert(input.back() == '$');

    return input;
}