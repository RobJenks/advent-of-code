#include "day24.h"
#include <iostream>
#include <numeric>
#include <sstream>
#include "../common/Test.h"


void Day24::Run(void) const
{
    std::cout << "\nDay 24:\n";

    RunTests();
    Part1();
}


void Day24::RunTests(void) const
{
    TestParsing();
    TestCombat();
    TestCombatPrecedence();
    TestSpecialStats();
}


void Day24::TestParsing(void) const
{
    std::cout << "Testing parsing:\n";
    std::vector<std::string> inputs = { ReadInput("day24/tests.txt"), ReadInput("day24/input.txt") };

    for (const auto & input : inputs)
    {
        auto combat = ParseInput(GetLines(input));
        Test::AssertComplexVerbose(combat.str_detail(), input + (input.back() == '\n' ? "" : "\n"), 
            "Parsing failure for input scenario", "Input parsing");
    }
}

void Day24::TestCombat(void) const
{
    std::cout << "Testing combat:\n";
    auto input = GetLines(ReadInput("day24/tests_combat.txt"));
    std::vector<std::vector<std::string>> phases = 
    {
        { input.cbegin() + 0, input.cbegin() + 6 },
        { input.cbegin() + 7, input.cbegin() + 12 },
        { input.cbegin() + 13, input.cbegin() + 18 },
        { input.cbegin() + 19, input.cbegin() + 24 },
        { input.cbegin() + 25, input.cbegin() + 30 },
        { input.cbegin() + 31, input.cbegin() + 36 },
        { input.cbegin() + 37, input.cbegin() + 42 },
        { input.cbegin() + 43, input.cbegin() + 48 },
        { input.cbegin() + 49, input.cbegin() + 54 }
    };

    auto group_input = GetLines(ReadInput("day24/tests.txt"));
    auto combat = ParseInput(group_input);
    Test::AssertComplexVerbose(combat.str_summary(), StringUtil::ConcatLines(phases[0]), "Invalid initial combat state", "Initial combat state");

    while (!combat.Complete())
    {
        combat.Execute();
        Test::AssertComplexVerbose(combat.str_summary(), StringUtil::ConcatLines(phases[combat.RoundsExecuted()]),
            "Invalid combat state after round " + std::to_string(combat.RoundsExecuted()), "Combat phase " + std::to_string(combat.RoundsExecuted()));
    }
}

void Day24::TestCombatPrecedence(void) const
{
    ImmuneCombat combat;
    combat.AddGroup(ArmyGroup(ArmyGroup::Faction::Immune, 100, 10000, 100, 1000, Damage::Type::Fire, { Damage::Type::Bludgeoning }, { Damage::Type::Fire }));
    combat.AddGroup(ArmyGroup(ArmyGroup::Faction::Immune, 100, 10000, 100, 1005, Damage::Type::Fire, { Damage::Type::Bludgeoning }, {}));
    combat.AddGroup(ArmyGroup(ArmyGroup::Faction::Immune, 100, 10000, 100, 1010, Damage::Type::Fire, { Damage::Type::Cold }, {}));
    combat.AddGroup(ArmyGroup(ArmyGroup::Faction::Immune, 100, 10000, 101, 1010, Damage::Type::Fire, { Damage::Type::Fire }, {}));
    combat.AddGroup(ArmyGroup(ArmyGroup::Faction::Infection, 100, 10000, 100, 300, Damage::Type::Fire, {}, {}));
    combat.AddGroup(ArmyGroup(ArmyGroup::Faction::Infection, 100, 10000, 100, 300, Damage::Type::Fire, {}, { Damage::Type::Fire }));
    combat.AddGroup(ArmyGroup(ArmyGroup::Faction::Infection, 100, 10000, 100, 200, Damage::Type::Fire, { Damage::Type::Fire }, {}));
    combat.AddGroup(ArmyGroup(ArmyGroup::Faction::Infection, 100, 10000, 100, 201, Damage::Type::Fire, { Damage::Type::Fire }, {}));
    combat.AddGroup(ArmyGroup(ArmyGroup::Faction::Infection, 100, 10000, 101, 201, Damage::Type::Fire, { Damage::Type::Fire }, {}));

    auto order = combat.GetEffectivePowerOrder();
    Test::AssertVectorVerbose(order, { 3,2,1,0,4,5,8,7,6 }, "Incorrect target selection order", "Target selection order");
    
    auto targets = combat.PerformTargetSelection();
    std::cout << "\nTargets:\n";
    for (auto i : order)
        std::cout << "Group " << i << " targeting " << targets[i].first << ", being targeted by " << targets[i].second << "\n";
    std::cout << "\n";

    // Immune groups select according to precendence rules; [5] is not eligible due to immunities
    Test::AssertVerbose(targets[3].first, 8, "Incorrect target selection", "Target selection");
    Test::AssertVerbose(targets[2].first, 7, "Incorrect target selection", "Target selection");
    Test::AssertVerbose(targets[1].first, 6, "Incorrect target selection", "Target selection");
    Test::AssertVerbose(targets[0].first, 4, "Incorrect target selection", "Target selection");
    
    // Infection groups also follow precedence rules; [0] is not eligible due to immunities, some groups remain untargeted due to unbalanced factions
    Test::AssertVerbose(targets[4].first, 3, "Incorrect target selection", "Target selection");
    Test::AssertVerbose(targets[5].first, 2, "Incorrect target selection", "Target selection");
    Test::AssertVerbose(targets[8].first, 1, "Incorrect target selection", "Target selection");
    Test::AssertVerbose(targets[7].first, -1, "Incorrect target selection", "Target selection");
    Test::AssertVerbose(targets[6].first, -1, "Incorrect target selection", "Target selection");
}

void Day24::TestSpecialStats(void) const
{
    auto attacker = ArmyGroup(ArmyGroup::Faction::Immune, 100, 100, 100, 100, Damage::Type::Fire, {}, {});
    auto defender = ArmyGroup(ArmyGroup::Faction::Infection, 100, 100, 100, 100, Damage::Type::Fire, {}, {});
    auto defender_immune = ArmyGroup(ArmyGroup::Faction::Infection, 100, 100, 100, 100, Damage::Type::Fire, {}, { Damage::Type::Fire });
    auto defender_weak = ArmyGroup(ArmyGroup::Faction::Infection, 100, 100, 100, 100, Damage::Type::Fire, { Damage::Type::Fire }, {});
    auto defender_really_weak = ArmyGroup(ArmyGroup::Faction::Infection, 100, 100, 100, 100, Damage::Type::Fire, { Damage::Type::Fire, Damage::Type::Cold, Damage::Type::Radiation }, {});

    Test::AssertVerbose(attacker.CalculateDamageTo(defender), attacker.EffectivePower(), "Incorrect attack damage", "Attack damage");
    Test::AssertVerbose(attacker.CalculateDamageTo(defender_immune), 0, "Incorrect attack damage", "Attack damage");
    Test::AssertVerbose(attacker.CalculateDamageTo(defender_weak), attacker.EffectivePower() * 2, "Incorrect attack damage", "Attack damage");
    Test::AssertVerbose(attacker.CalculateDamageTo(defender_really_weak), attacker.EffectivePower() * 2, "Incorrect attack damage", "Attack damage");

    Test::AssertVerbose(defender.CalculateDamageFrom(attacker), attacker.EffectivePower(), "Incorrect attack damage", "Attack damage");
    Test::AssertVerbose(defender_immune.CalculateDamageFrom(attacker), 0, "Incorrect attack damage", "Attack damage");
    Test::AssertVerbose(defender_weak.CalculateDamageFrom(attacker), attacker.EffectivePower() * 2, "Incorrect attack damage", "Attack damage");
    Test::AssertVerbose(defender_really_weak.CalculateDamageFrom(attacker), attacker.EffectivePower() * 2, "Incorrect attack damage", "Attack damage");
}

void Day24::Part1(void) const
{
    std::cout << "Part 1:\n\n";
    auto input = GetLines(ReadInput("day24/input.txt"));

    auto combat = ParseInput(input);
    std::cout << "Initial state:\n\n" << combat.str_detail() << "\n";

    while (!combat.Complete())
    {
        combat.Execute();
    }

    std::cout << "\nFinal state:\n\n" << combat.str_detail() << "\n\n";
    int result = std::accumulate(combat.GetGroups().cbegin(), combat.GetGroups().cend(), 0, [](int acc, const ArmyGroup & el) {
        return (acc + el.GetUnitCount());
    });

    std::cout << "Part 1 result: " << result << " (after " << combat.RoundsExecuted() << " rounds)\n";
}



ImmuneCombat Day24::ParseInput(const std::vector<std::string> & input) const
{
    ImmuneCombat combat;

    ArmyGroup::Faction current_faction = ArmyGroup::Faction::Unknown;
    int count, hp, attack, initiative;
    Damage::Type damage_type;
    std::vector<Damage::Type> weak, immune;
    std::string word;

    for (const auto & in_line : input)
    {
        std::string line = StringUtil::Trim(in_line);
        if (line.empty()) continue;

        if (line == "Immune System:") { current_faction = ArmyGroup::Faction::Immune; continue; }
        if (line == "Infection:") { current_faction = ArmyGroup::Faction::Infection; continue; }

        std::stringstream ss(line);
        ss >> count >> word >> word >> word >> hp;

        ss = std::stringstream(line.substr(line.find("that does")));
        ss >> word >> word >> attack >> word;
        damage_type = Damage::FromString(word);
        
        ss >> word >> word >> word >> initiative;

        // Process special properties, e.g. immunities or weaknesses, if any are present
        auto spec_start = line.find('(');
        if (spec_start != std::string::npos)
        {
            auto [_weak, _immune] = ParseSpecialProperties(line.substr(spec_start, line.find(')') - spec_start + 1));
            weak = _weak; immune = _immune;
        }
        else
        {
            weak.clear(); immune.clear();
        }

        combat.AddGroup(ArmyGroup(current_faction, count, hp, initiative, attack, damage_type, weak, immune));
    }

    return combat;
}

std::pair<std::vector<Damage::Type>, std::vector<Damage::Type>> Day24::ParseSpecialProperties(const std::string & input) const
{
    std::string word;
    std::vector<Damage::Type> weak;
    std::vector<Damage::Type> immune;

    // Can have multiple groups of special stat
    std::vector<std::string> parts;
    auto semi = input.find(';');
    if (semi != std::string::npos)
    {
        parts.push_back(input.substr(1U, semi - 1U));
        parts.push_back(input.substr(semi + 1U, (input.size() - semi) - 1U));
    }
    else
    {
        parts.push_back(input.substr(1U, input.size() - 2U));
    }

    // Parse the group
    bool is_weakness;
    for (const std::string & spec : parts)
    {
        std::stringstream ss(spec);
        ss >> word;

        if (word == "immune") is_weakness = false;
        else if (word == "weak") is_weakness = true;
        else assert(false);

        ss >> word;
        while (ss)
        {
            ss >> word;
            auto cm = word.find_first_of(",)");
            if (cm != std::string::npos) word[cm] = ' ';

            std::vector<Damage::Type> & vec = (is_weakness ? weak : immune);
            auto stat = Damage::FromString(StringUtil::Trim(word));
            
            if (std::find(vec.cbegin(), vec.cend(), stat) != vec.end()) continue;   // To allow for input formatting being inconsistent
            vec.push_back(stat);
        }
    }

    return { weak, immune };
}




