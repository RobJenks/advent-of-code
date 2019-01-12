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
    Part2();
}

void Day15::RunTests(void) const
{
    TestMapLoading();
    TestReadingOrder();
    TestSampleMovement();
    TestSampleCombat();
    TestFullScenarios();
    TestAttackPower();
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
    Test::AssertComplexVerbose(combat.str(), StringUtil::ConcatLines(tests[0]));

    for (size_t i = 1U; i < 4; ++i)
    {
        std::cout << "After " << i << " round" << (i != 1 ? "s" : "") << ":\n";
        combat.Execute();

        Test::AssertComplexVerbose(combat.str(), StringUtil::ConcatLines(tests[i]));
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

        std::cout << "\nState after round " << combat.GetRoundNumber() << ":\n";
        std::cout << combat.str() << "\n";

        auto remaining_hp = combat.GetRemainingActorHp();
        Test::AssertIterableVerbose(remaining_hp, it->second, "Incorrect remaining HP at round " + std::to_string(it->first), "Remaining HP");

        auto outcome = combat.CalculateOutcome();
        auto expected = (static_cast<long>(it->first) * std::accumulate(it->second.cbegin(), it->second.cend(), 0L));
        
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
        auto exp_start = StringUtil::ConcatLines(start);
        auto exp_end = StringUtil::ConcatLines(std::vector<std::string>(input.begin() + std::get<1>(test).first, input.begin() + std::get<1>(test).second));

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

void Day15::TestAttackPower(void) const
{
    std::vector<std::string> input = GetLines(ReadInput("day15/tests-attackpower.txt"));

    // { start_range, end_range, attack_power, expected_rounds, expected_remaining_hp, expected_outcome }
    typedef std::tuple<std::pair<size_t, size_t>, std::pair<size_t, size_t>, int, int, int, long> Scenario;
    std::vector<Scenario> test_scenarios = {
        { { 0,  7}, { 8, 15}, 15, 29, 172, 4988L },
        { {16, 23}, {24, 31}, 4, 33, 948, 31284L },
        { {32, 39}, {40, 47}, 15, 37, 94, 3478L },
        { {48, 55}, {56, 63}, 12, 39, 166, 6474L },
        { {64, 73}, {74, 83}, 34, 30, 38, 1140L }
    };

    int scenario = 0;
    for (const auto & test : test_scenarios)
    {
        auto start = std::vector<std::string>(input.begin() + std::get<0>(test).first, input.begin() + std::get<0>(test).second);
        auto exp_start = StringUtil::ConcatLines(start);
        auto exp_end = StringUtil::ConcatLines(std::vector<std::string>(input.begin() + std::get<1>(test).first, input.begin() + std::get<1>(test).second));

        std::cout << "\nAttack power test " << ++scenario << " (with power = " << std::get<2>(test) << "):\n";

        Combat combat(start);
        Test::AssertComplexVerbose(combat.str(), exp_start, "Initial state is not correct", "Attack power scenario " + std::to_string(scenario) + " initial state");

        auto starting_actors = combat.GetActiveActorsOfClass(Actor::Class::Elf).size();

        combat.SetFactionAttackStrength(Actor::Class::Elf, std::get<2>(test));
        while (!combat.HasTerminated())
        {
            combat.Execute();
        }

        auto hp = combat.GetRemainingActorHp();
        int remaining_hp = std::accumulate(hp.cbegin(), hp.cend(), 0);

        Test::AssertVerbose(Actor::ClassString(combat.GetRemainingActors().at(0)->GetClass()), Actor::ClassString(Actor::Class::Elf), "Unexpected combat winners", "Combat winner");

        // Assert all scenario results together
        {
            Test::DeferredAssertionScope scope;

            Test::AssertComplexVerbose(combat.str(), exp_end, "Final scenario state is not correct", "Attack power scenario " + std::to_string(scenario) + " final state");
            Test::AssertVerbose(combat.GetActiveActorsOfClass(Actor::Class::Elf).size(), starting_actors, "Casualties were not expected", "Attack power scenario " + std::to_string(scenario) + " survivors");
            Test::AssertVerbose(combat.GetRoundNumber(), std::get<3>(test), "Incorrect final scenario round number", "Attack power scenario " + std::to_string(scenario) + " final round number");
            Test::AssertVerbose(remaining_hp, std::get<4>(test), "Incorrect HP for remaining actors", "Attack power scenario " + std::to_string(scenario) + " remaining HP");
            Test::AssertVerbose(combat.CalculateOutcome(), std::get<5>(test), "Incorect scenario outcome", "Attack power scenario " + std::to_string(scenario) + " final outcome");
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

    std::cout << "\nPart 1 final state:\n\n" << combat.str() << "\n\n"; 
    
    auto result = combat.CalculateOutcome();
    std::cout << "\nPart 1 result: " << result << " (after " << combat.GetRoundNumber() << " rounds)\n";
}


void Day15::Part2(void) const
{
    std::vector<std::string> input = GetLines(ReadInput("day15/input.txt"));

    int str_low = 3;
    int str_high = str_low;
    bool no_elf_casualties = false;

    std::cout << "\nPart 2 - Establishing upper bound:\n";
    while (!no_elf_casualties)
    {
        str_high *= 2;  // Double high estimate each time
        std::cout << "Testing with combat strength " << str_high;

        Combat combat(input);
        no_elf_casualties = EvaluateToConclusionWithoutCasualties(combat, Actor::Class::Elf, str_high);
        std::cout << "; result = " << (no_elf_casualties ? "no casualties" : "casualties") << "\n";
    }

    std::cout << "\nPart 2 - evaluating search space (" << str_low << " " << str_high << "]\n";
    
    // Binary search the range for two consecutive entries ({ Casualties, NoCasualties })
    Combat combat; int str;
    std::unordered_map<int, bool> cache;
    while (true)
    {
        str = (str_low + str_high) / 2;
        std::cout << "Testing with combat strength " << str-1 << "/" << str;

        combat = Combat(input);
        bool no_casualties = (cache.count(str-1) != 0 ? cache[str-1] : EvaluateToConclusionWithoutCasualties(combat, Actor::Class::Elf, str-1));
        cache[str - 1] = no_casualties;
        if (no_casualties)
        {
            // If we take no casualties at the lower values then no need to test the upper; move towards upper bound
            std::cout << "; result = no casualties at " << str-1 << ", skipping test of " << str << "\n";
            str_high = str-1;
            continue;
        }
        
        // Otherwise, first entry is 'casualties' and we should evaluate the second entry
        combat = Combat(input);
        no_casualties = (cache.count(str) != 0 ? cache[str] : EvaluateToConclusionWithoutCasualties(combat, Actor::Class::Elf, str));
        cache[str] = no_casualties;
        if (!no_casualties)
        {
            // We took casualties in both entries, so move towards upper bound
            std::cout << "; result = casualties at " << str-1 << " and " << str << "\n";
            str_low = str;
            continue;
        }

        // This is the break point between casualties & no casualties
        std::cout << "; result = casualties at " << str-1 << ", no casualties at " << str << ", breakpoint identified\n";
        std::cout << "Final combat state:\n\n" << combat.str() << "\n\n";
        break;
    }

    std::cout << "\nPart 2 result: " << combat.CalculateOutcome() << " (at combat strength " << str << ", after " << combat.GetRoundNumber() << " rounds)\n";
}

// Evaluate a full combat scenario with the given attack strength for a faction.  Executes the 
// scenario to termination and returns a value indicating whether the faction won with zero casualties
bool Day15::EvaluateToConclusionWithoutCasualties(Combat & combat, Actor::Class faction, int combat_strength) const
{
    combat.SetFactionAttackStrength(faction, combat_strength);
    auto count = combat.GetActiveActorsOfClass(faction).size();

    while (!combat.HasTerminated())
    {
        combat.Execute();
    }

    // Make sure the target faction won
    if (combat.GetRemainingActors().at(0)->GetClass() != faction) return false;

    // Check for casualties
    return (combat.GetActiveActorsOfClass(faction).size() == count);
}