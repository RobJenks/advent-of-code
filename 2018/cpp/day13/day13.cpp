#include "day13.h"
#include <iostream>
#include <array>
#include <vector>
#include <numeric>
#include <algorithm>
#include <cassert>


void Day13::Run(void) const
{
    std::cout << "\nDay 13:\n";

    RunTests();
    Part1();
    Part2();
}

void Day13::RunTests(void) const
{
    std::vector<std::string> input = GetLines(ReadInput("day13/tests.txt"));

    std::array<std::vector<std::string>, 15U> steps = 
    {
        std::vector<std::string>(input.begin() + 0, input.begin() + 6),
        std::vector<std::string>(input.begin() + 7, input.begin() + 13),
        std::vector<std::string>(input.begin() + 14, input.begin() + 20),
        std::vector<std::string>(input.begin() + 21, input.begin() + 27),
        std::vector<std::string>(input.begin() + 28, input.begin() + 34),
        std::vector<std::string>(input.begin() + 35, input.begin() + 41),
        std::vector<std::string>(input.begin() + 42, input.begin() + 48),
        std::vector<std::string>(input.begin() + 49, input.begin() + 55),
        std::vector<std::string>(input.begin() + 56, input.begin() + 62),
        std::vector<std::string>(input.begin() + 63, input.begin() + 69),
        std::vector<std::string>(input.begin() + 70, input.begin() + 76),
        std::vector<std::string>(input.begin() + 77, input.begin() + 83),
        std::vector<std::string>(input.begin() + 84, input.begin() + 90),
        std::vector<std::string>(input.begin() + 91, input.begin() + 97),
        std::vector<std::string>(input.begin() + 98, input.begin() + 104),
    };

    Tracks tracks = BuildTracks(steps[0]);
    tracks.UpdateTrackStateWithCrashes();
    VerifyTestState(tracks, steps[0]);       

    std::cout << tracks.str() << "\n";

    for (size_t i = 1U; i < steps.size(); ++i)
    {
        std::cout << "Simulating step " << i << "\n";

        tracks.Simulate(); std::cout << tracks.str() << "\n";
        VerifyTestState(tracks, steps[i]);
    }


}

void Day13::Part1(void) const
{
    std::vector<std::string> input = GetLines(ReadInput("day13/input.txt"));

    Tracks tracks = BuildTracks(input);
    tracks.UpdateTrackStateWithCrashes();
    tracks.VerifyState();

    while (!tracks.CrashesHaveOccurred())
    {
        tracks.Simulate();
    }

    std::cout << "Part 1:" << tracks.str() << "\n\n";
    std::cout << "Part 1 result: " << tracks.Coord(tracks.GetCrashLocations().at(0)).str() << " (after " << tracks.GetCycleCount() << " cycles)\n";
}

void Day13::Part2(void) const
{
    std::vector<std::string> input = GetLines(ReadInput("day13/input.txt"));

    Tracks tracks = BuildTracks(input);
    tracks.TerminateAtLastCar();
    tracks.VerifyState();

    while (!tracks.HasTerminated())
    {
        tracks.Simulate();
    }
    
    auto active = tracks.GetActiveCars();
    assert(active.size() == 1U);

    std::cout << "Part 2 result: " << tracks.Coord(active.at(0).GetCell()).str() << " (after " << tracks.GetCycleCount() << " cycles)\n";
}


Tracks Day13::BuildTracks(const std::vector<std::string> & input) const
{
    // Generate grid (Length of longest line x number of non-empty lines)
    Vec2<int> size(
        std::accumulate(input.begin(), input.end(), static_cast<int>(0), [](int acc, const auto & el) {
            return std::max(acc, static_cast<int>(el.size()));
        }),
        static_cast<int>(std::count_if(input.begin(), input.end(), [](const auto & el) { return !el.empty(); }))
    );

    Tracks tracks(size);

    // Populate from input data
    for (int y = 0; y < input.size(); ++y)
    {
        const auto & line = input[y];

        for (int x = 0; x < line.size(); ++x)
        {
            auto index = tracks.Index(x, y);
            auto state = Cell::GetConnectionsFromSchematic(line[x]);

            if (state == Cell::States::UnconfirmedCarCell)
            {
                tracks.RegisterCar(index, line[x]);
            }
            else
            {
                tracks.Data[index].AddConnections(state);
            }
        }
    }

    // Post-process any cells which are dependent on other cells
    tracks.PerformPostProcessing();

    return tracks;
}

bool Day13::VerifyTestState(const Tracks & tracks, const std::vector<std::string> & expected) const
{
    std::string actual = tracks.str();

    std::stringstream ss;
    for (size_t i = 0U; i < expected.size(); ++i)
    {
        ss << expected[i] << '\n';
    }

    std::string exp = ss.str();
    if (actual != exp)
    {
        std::cout << "Test state is not correct; expected:\n";
        for (const auto & str : expected) std::cout << str << '\n';

        std::cout << "\nActual:\n" << actual << "\n";

        assert(false);
        return false;
    }

    return true;
}