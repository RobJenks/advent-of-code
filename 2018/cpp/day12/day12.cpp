#include "day12.h"
#include <iostream>
#include <tuple>
#include <sstream>
#include "../common/StringUtil.h"

void Day12::Run(void) const
{
    std::cout << "\nDay 12:\n";

    RunTests();
    Part1();
}

void Day12::RunTests(void) const
{
    std::vector<std::string> input = GetLines(ReadInput("day12/tests.txt"));
    auto [state, rules] = ParseInput(input);

    std::cout << "\nTest:\n";
    RunSimulationVerbose(state, rules, 20U);

    int expected = 325;
    int score = state.DetermineStateScore();

    std::cout << "Result: " << score << ", expected: " << expected << " (" << (score == expected ? "Pass" : "FAIL") << ")\n";
}

void Day12::Part1(void) const
{
    std::vector<std::string> input = GetLines(ReadInput("day12/input.txt"));
    auto[state, rules] = ParseInput(input);

    std::cout << "\nPart 1:\n";
    RunSimulationVerbose(state, rules, 20U);

    int result = state.DetermineStateScore();
    std::cout << "Result: " << result << "\n";
}

std::pair<State, std::vector<Rule>> Day12::ParseInput(const std::vector<std::string> & input) const
{
    std::string str;

    // First line contains initial state
    std::stringstream ss(input[0]);
    ss >> str >> str >> str;
    State state(str);

    // All other non-empty lines contain rules
    std::vector<Rule> rules;
    std::for_each(input.begin() + 1, input.end(), [&rules](const auto & el) 
    {
        std::string s = StringUtil::Trim(el);
        if (s.empty()) return;

        Rule rule(s);
        if (rule.Result) rules.push_back(rule); // Only need to store rules with output; default rule if no match -> '.'
    });

    return { state, rules };
}

void Day12::RunSimulationVerbose(State & state, const std::vector<Rule> & rules, unsigned int iterations) const
{
    std::cout << "0: " << state.str(true) << "\n";
    for (unsigned int i = 0U; i < iterations; ++i)
    {
        state.ApplyRules(rules);
        std::cout << i + 1 << ": " << state.str(true) << "\n";
    }
}