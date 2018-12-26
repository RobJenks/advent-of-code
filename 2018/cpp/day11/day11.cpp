#include "day11.h"
#include <iostream>
#include "Grid.h"

void Day11::Run(void) const
{
    std::cout << "\nDay 11:\n";

    RunTests();
    Part1();
    Part2();
}

void Day11::Part1(void) const
{
    Grid<300U, 300U> grid(PROBLEM_INPUT);

    auto result = grid.GetHighestPowerGroup(3);
    std::cout << "Part 1 result: " << std::get<0>(result).str() << " (power level = " << std::get<1>(result) << ")\n";
}

void Day11::Part2(void) const
{
    Grid<300U, 300U> grid(PROBLEM_INPUT);

    auto result = grid.GetHighestPowerGroupInRange(300);
    std::cout << "\nPart 2 result: " << std::get<0>(result).str() << ", group size = " << std::get<2>(result) << " (power level = " << std::get<1>(result) << ")\n";
}

void Day11::RunTests(void) const
{
    RunPowerTests();
    RunGroupTests();
    RunRangeTests();
}

void Day11::RunPowerTests(void) const
{
    std::vector<std::tuple<Vec2<int>, int, int>> tests =
    {
        { { 122, 79}, 57, -5 },     // Fuel cell at 122, 79, grid serial number 57: power level -5.
        { { 217, 196 }, 39, 0 },    // Fuel cell at 217,196, grid serial number 39: power level  0.
        { { 101, 153 }, 71, 4 }     // Fuel cell at 101,153, grid serial number 71: power level  4.
    };

    for (const auto & test : tests)
    {
        Vec2 coord = std::get<0>(test);
        std::cout << "Test: Grid serial = " << std::get<1>(test) << ", power level at (" << coord.x <<
            ", " << coord.y << ") expected to be " << std::get<2>(test) << "\n";

        Grid<300, 300> grid(std::get<1>(test));
        int power = grid.Power(coord.x, coord.y);
        std::cout << "Result: " << power << (power == std::get<2>(test) ? " [Pass]" : " [FAIL]") << "\n\n";
    }
}

void Day11::RunGroupTests(void) const
{
    std::vector<std::tuple<int, Vec2<int>, int>> tests =
    {
        {18, {33, 45}, 29}, // For grid serial number 18, the largest 3x3 square's top-left is 33,45 (with a total power of 29)
        {42, {21, 61}, 30}  // For grid serial number 42, the largest 3x3 square's top-left is 21,61 (with a total power of 30)
    };

    for (const auto & test : tests)
    {
        std::cout << "Test: Grid serial = " << std::get<0>(test) << ", highest power group expected to be at "
            << std::get<1>(test).str() << " with power level " << std::get<2>(test) << "\n";

        Grid<300, 300> grid(std::get<0>(test));
  
        auto result = grid.GetHighestPowerGroup(3);

        std::cout << "Result: " << std::get<0>(result).str() << " (Power " << std::get<1>(result) << ")" <<
            ((std::get<1>(test) == std::get<0>(result) &&
              std::get<2>(test) == std::get<1>(result))
                ? " (Pass)" : " (FAIL)") << "\n\n";
    }
}

void Day11::RunRangeTests(void) const
{
    std::vector<std::tuple<int, Vec2<int>, int, int>> tests =
    {
        {18, {90, 269}, 113, 16},  // For grid serial number 18, the largest square (with a total power of 113) is 16x16 and has a top-left corner of 90,269
        {42, {232, 251}, 119, 12}  // For grid serial number 42, the largest square (with a total power of 119) is 12x12 and has a top-left corner of 232,251
    };

    for (const auto & test : tests)
    {
        std::cout << "Test: Grid serial = " << std::get<0>(test) << ", highest power group expected to be at "
            << std::get<1>(test).str() << " with size " << std::get<3>(test) << " and power level " << std::get<2>(test) << "\n";

        Grid<300, 300> grid(std::get<0>(test));

        auto result = grid.GetHighestPowerGroupInRange(300);

        std::cout << "Result: " << std::get<0>(result).str() << " with size " << std::get<2>(result) << " (Power " << std::get<1>(result) << ")" <<
            ((std::get<1>(test) == std::get<0>(result) &&
                std::get<3>(test) == std::get<2>(result) && 
                std::get<2>(test) == std::get<1>(result))
                ? " (Pass)" : " (FAIL)") << "\n\n";
    }
}
