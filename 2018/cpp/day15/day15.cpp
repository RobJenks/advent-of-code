#include "day15.h"
#include <iostream>
#include <vector>
#include <unordered_set>
#include <unordered_map>
#include <numeric>
#include <string>
#include <cassert>
#include "../common/Test.h"
#include "../common/StringUtil.h"
#include "Combat.h"
#include "RO.h"

void Day15::Run(void) const
{
    std::cout << "\nDay 15:\n";
    
    RunTests();
    Part1();
}

void Day15::RunTests(void) const
{
    TestMapLoading();
    TestReadingOrder();
    TestSampleMovement();
    TestSampleCombat();
    TestFullScenarios();
}

void Day15::TestMapLoading(void) const
{
    std::string input = ReadInput("day15/input.txt");

    Combat combat(GetLines(input));
    std::string map = combat.str();

    std::cout << "Map:\n\n" << map << '\n';

    if (map != input)
    {
        std::cout << "Does not match input schematic:\n\n" << input << '\n';
        assert(false);
    }
}

void Day15::TestReadingOrder(void) const
{
    Test::AssertVerbose(static_cast<size_t>(RO::ObjVal(120U, 125U)), static_cast<size_t>(120U));
    Test::AssertVerbose(static_cast<size_t>(RO::ObjVal(125U, 120U)), static_cast<size_t>(120U));
    Test::AssertVerbose(static_cast<size_t>(RO::ObjVal(125U, 125U)), static_cast<size_t>(125U));
    
    Test::AssertVerbose(RO::Coords({ 1,2 }, { 2,4 }, 5), { 1,2 });
    Test::AssertVerbose(RO::Coords({ 2,4 }, { 1,2 }, 5), { 1,2 });
    Test::AssertVerbose(RO::Coords({ 4,4 }, { 3,4 }, 5), { 3,4 });
    
    Tile t0(12, { 2, 1 }), t1(23, { 3, 2 });
    Test::AssertVerbose(RO::Objects(t0, t1), t0);
    Test::AssertVerbose(RO::Objects(t1, t0), t0);
    Test::AssertVerbose(RO::Objects(t1, t1), t1);

    Test::AssertVectorVerbose(RO::Vector<Tile>({ t0, t1, t0, t1 }), { t0, t0, t1, t1 });
    Test::AssertVectorVerbose(RO::Vector<size_t>({ 56, 34, 78, 12 }), { 12, 34, 56, 78 });
}

void Day15::TestSampleMovement(void) const
{
    std::vector<std::string> input = GetLines(ReadInput("day15/tests-movement.txt"));

    std::array<std::vector<std::string>, 4U> tests = 
    {
        std::vector<std::string>(input.begin() + 0, input.begin() + 9),
        std::vector<std::string>(input.begin() + 10, input.begin() + 19),
        std::vector<std::string>(input.begin() + 20, input.begin() + 29),
        std::vector<std::string>(input.begin() + 30, input.begin() + 39)
    };
    
    std::cout << "\nSample movement test:\n\n";

    Combat combat(tests[0]);

    std::cout << "Initial state:\n";
    Test::AssertComplexVerbose(combat.str(), StringUtil::Concat(tests[0]));

    for (size_t i = 1U; i < 4; ++i)
    {
        std::cout << "After " << i << " round" << (i != 1 ? "s" : "") << ":\n";
        combat.Execute();

        Test::AssertComplexVerbose(combat.str(), StringUtil::Concat(tests[i]));
    }
}

void Day15::TestSampleCombat(void) const
{
    std::vector<std::string> input = GetLines(ReadInput("day15/tests-combat.txt"));
    std::unordered_map<int, std::unordered_multiset<int>> expected_hp = {
        { 1,  { 200, 200, 197, 197, 197, 197 } },
        { 2,  { 200, 200, 194, 194, 194, 188 } },
        { 23, { 200, 200, 131, 131, 131 } },
        { 24, { 200, 200, 131, 128, 128 } },
        { 25, { 200, 200, 131, 125, 125 } },
        { 26, { 200, 200, 131, 122, 122 } },
        { 27, { 200, 200, 131, 119, 119 } },
        { 28, { 200, 200, 131, 116, 113 } },
        { 47, { 200, 200, 131, 59 } }
    };

    std::cout << "\nSample combat test:\n\n";

    Combat combat(input);
    while (!combat.HasTerminated())
    {
        combat.Execute();

        auto it = expected_hp.find(combat.GetRoundNumber());
        if (it == expected_hp.end()) continue;

        std::cout << "State after round " << combat.GetRoundNumber() << ":\n";
        std::cout << combat.str() << "\n";

        auto remaining_hp = combat.GetRemainingActorHp();
        Test::AssertIterableVerbose(remaining_hp, it->second, "Incorrect remaining HP at round " + std::to_string(it->first), "Remaining HP");

        auto outcome = combat.CalculateOutcome();
        auto expected = (static_cast<long>(it->first) * std::accumulate(it->second.cbegin(), it->second.cend(), 0L));
        //std::cout << "Outcome: " << outcome << " (Expected: " << expected << ")\n\n";
        Test::AssertVerbose(outcome, expected, "Incorrect outcome at round " + std::to_string(it->first), "Outcome");
    }

    Test::AssertVerbose(combat.HasTerminated(), true, "Combat has not correctly terminated");
    Test::AssertVerbose(combat.GetRoundNumber(), 47, "Incorrect ending round number");
}

void Day15::TestFullScenarios(void) const
{
    std::vector<std::string> input = GetLines(ReadInput("day15/tests-scenarios.txt"));

    // { start_range, end_range, expected_rounds, expected_outcome }
    typedef std::tuple<std::pair<size_t, size_t>, std::pair<size_t, size_t>, int, long> Scenario;   
    std::vector<Scenario> test_scenarios = {
        { { 0,  7}, { 8, 15}, 37, 36334L },
        { {16, 23}, {24, 31}, 46, 39514L },
        { {32, 39}, {40, 47}, 35, 27755L }, 
        { {48, 55}, {56, 63}, 54, 28944L },
        { {64, 73}, {74, 83}, 20, 18740L }
    };

    int scenario = 0;
    for (const auto & test : test_scenarios)
    {
        auto start = std::vector<std::string>(input.begin() + std::get<0>(test).first, input.begin() + std::get<0>(test).second);
        auto exp_start = StringUtil::Concat(start);
        auto exp_end = StringUtil::Concat(std::vector<std::string>(input.begin() + std::get<1>(test).first, input.begin() + std::get<1>(test).second));

        std::cout << "\nScenario test " << ++scenario << ":\n";
        
        Combat combat(start);
        Test::AssertComplexVerbose(combat.str(), exp_start, "Initial scenario state is not correct", "Scenario " + std::to_string(scenario) + " initial state");

        while (!combat.HasTerminated())
        {
            combat.Execute();
        }

        // Assert all scenario results together
        {
            Test::DeferredAssertionScope scope;

            Test::AssertComplexVerbose(combat.str(), exp_end, "Final scenario state is not correct", "Scenario " + std::to_string(scenario) + " final state");
            Test::AssertVerbose(combat.GetRoundNumber(), std::get<2>(test), "Incorrect final scenario round number", "Scenario " + std::to_string(scenario) + " final round number");
            Test::AssertVerbose(combat.CalculateOutcome(), std::get<3>(test), "Incorect scenario outcome", "Scenario " + std::to_string(scenario) + " final outcome");
        }
    }
}

void Day15::Part1(void) const
{
    std::vector<std::string> input = GetLines(ReadInput("day15/input.txt"));

    Combat combat(input);
    while (!combat.HasTerminated())
    {
        combat.Execute();
    }

    auto result = combat.CalculateOutcome();
    std::cout << "Part 1 result: " << result << " (after " << combat.GetRoundNumber() << " rounds)\n";
}
