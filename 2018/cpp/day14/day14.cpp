#include "day14.h"
#include <iostream>
#include <cassert>
#include "../common/StringUtil.h"
#include "Recipes.h"


void Day14::Run(void) const
{
    std::cout << "\nDay 14:\n";

    RunTests();
    Part1();
}

void Day14::RunTests(void) const
{
    RunStateTests();
    RunSequenceTests();
}

void Day14::RunStateTests(void) const
{
    std::vector<std::string> input = GetLines(ReadInput("day14/tests.txt"));

    std::cout << "\nTests:\n\n";
    Recipes<2U> scores(INITIAL_STATE);
    std::cout << scores.str() << '\n';
    assert(scores.str() == input[0]);

    for (auto it = input.cbegin() + 1; it < input.cend(); ++it)
    {
        scores.EvaluateIterations(1U);

        auto exp = StringUtil::Trim(*it);
        auto str = StringUtil::Trim(scores.str());
        std::cout << str << '\n';

        if (str != exp)
        {
            std::cout << "\n\nState does not match expected result\n";
            std::cout << "Expected: " << exp << '\n';
            std::cout << "Actual:   " << str << '\n';
            assert(false);
        }
    }
}

void Day14::RunSequenceTests(void) const
{
    const std::array<std::pair<size_t, std::string>, 3U> expected_ranges = {
        std::make_pair(5U, "0124515891"),
        std::make_pair(18U, "9251071085"),
        std::make_pair(2018U, "5941429882")
    };

    for (const auto & test : expected_ranges)
    {
        Recipes<2U> scores(INITIAL_STATE);
        scores.EvaluateToRecipeCount(test.first + 10U);

        auto actual = scores.str_compact(test.first, 10U);
        std::cout << "At recipe count " << test.first << ", following sequence = " <<
            actual << " (expected = " << test.second << ")\n";
        assert(actual == test.second);
    }
}

void Day14::Part1(void) const
{
    Recipes<2U> scores(INITIAL_STATE);

    scores.EvaluateToRecipeCount(TARGET_RECIPES + 10U);
    auto result = scores.str_compact(TARGET_RECIPES, 10U);

    std::cout << "\nPart 1 result: " << result << "\n";
}
