#include "day12.h"
#include <iostream>
#include <tuple>
#include <sstream>
#include <unordered_map>
#include "../common/StringUtil.h"

void Day12::Run(void) const
{
    std::cout << "\nDay 12:\n";

    RunTests();
    Part1();
    Part2();
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
    auto [state, rules] = ParseInput(input);

    std::cout << "\nPart 1:\n";
    RunSimulationVerbose(state, rules, 20U);

    int result = state.DetermineStateScore();
    std::cout << "Result: " << result << "\n";
}

void Day12::Part2(void) const
{
    std::vector<std::string> input = GetLines(ReadInput("day12/input.txt"));
    auto [state, rules] = ParseInput(input);

    std::cout << "\nPart 2:\n";
    
    // Map (ActivePattern -> { iteration, score })
    std::unordered_map<std::string , std::vector<std::pair<int, int>>> data;
    
    for (int iteration = 0; true; ++iteration)
    {
        auto score = state.DetermineStateScore();
        auto pattern = state.GetActivePattern();
        auto & vec = data[pattern];
        vec.push_back({ iteration, score });

        if (vec.size() >= 3)
        {
            std::cout << "Repeated pattern: " << pattern << "\n";
            for (int i = 0; i < vec.size(); ++i)
            {
                std::cout << "Iteration " << vec[i].first << ", Score: " << vec[i].second << "\n";
            }

            int it_diff[2] = { vec[1].first - vec[0].first, vec[2].first - vec[1].first };
            int diff[2] = { vec[1].second - vec[0].second, vec[2].second - vec[1].second };
            assert(it_diff[0] == it_diff[1] && diff[0] == diff[1]);

            std::cout << "Repeated score difference of " << diff[0] << " every " << it_diff[0] << " iterations, from iteration " << vec[0].first << "\n";
            std::cout << "Score at iteration 50,000,000,000 == " << vec[0].second << " + (((5e9 - " << vec[0].first << ") / " << it_diff[0] << ") * " << diff[0] <<
                ") == " << (vec[0].second + (((50000000000 - vec[0].first) / it_diff[0]) * diff[0])) << "\n\n";
            return;
        }

        state.ApplyRules(rules);
    }
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
