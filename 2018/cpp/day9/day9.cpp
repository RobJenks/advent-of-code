#include "day9.h"
#include <iostream>
#include <sstream>
#include <numeric>
#include "Board.h"


void Day9::Run(void) const
{
    std::cout << "\nDay 9:\n";

    RunTests();
    Part1();
    Part2();
}

void Day9::Part1(void) const
{
    std::string input = ReadInput(fs::path("day9/input.txt"));
    GameSetup setup = ParseInput(input);

    auto result = CalculateHighScore(setup);
    std::cout << "Part 1 result = " << result << "\n";
}

void Day9::Part2(void) const
{
    std::string input = ReadInput(fs::path("day9/input.txt"));
    GameSetup setup = ParseInput(input);

    setup.LastMarble *= 100;

    auto result = CalculateHighScore(setup);
    std::cout << "Part 2 result = " << result << "\n";
}

void Day9::RunTests(void) const
{
    std::vector<std::string> input = GetLines(ReadInput(fs::path("day9/tests.txt")));
    std::vector<Score> expected = { 8317, 146373, 2764, 54718, 37305 };

    for (int test_index = 0; test_index < input.size(); ++test_index)
    {
        const auto in = input[test_index];

        GameSetup setup = ParseInput(in);
        std::cout << "Test: " << in << "\n";

        auto result = CalculateHighScore(setup);
        std::cout << "Result: " << result << (result == expected[test_index] ? " (Pass)" : " (FAIL)") << "\n\n";
    }
}

Day9::GameSetup Day9::ParseInput(const std::string & input) const
{
    int players, lastmarble;
    std::string sink;

    std::stringstream ss(input);
    ss >> players >> sink >> sink >> sink >> sink >> sink >> lastmarble;

    return GameSetup(players, lastmarble);
}

Day9::Score Day9::CalculateHighScore(GameSetup setup) const
{
    Scoreboard scores = PlayGameToMarbleValue(setup);

    auto highscore = std::accumulate(scores.begin(), scores.end(), static_cast<Score>(0), [](Score acc, Score el) { return std::max(acc, el); });
    return highscore;
}
Day9::Scoreboard Day9::NewScoreboard(int players) const
{
    Scoreboard scoreboard;
    scoreboard.insert(scoreboard.begin(), players, static_cast<Score>(0));

    return scoreboard;
}

// Plays the game to the given final marble value, returning the score of each player
Day9::Scoreboard Day9::PlayGameToMarbleValue(GameSetup setup) const
{
    Board board;
    Scoreboard scores = NewScoreboard(setup.Players);

    int player = 0;
    int marble = 0;

    std::cout << "Calculating: ";
    while (true)
    {
        marble += 1;
        if (marble % (setup.LastMarble / 10) == 0) std::cout << ".";

        if (marble % 23 != 0)
        {
            //bool fwd = (board.IndexOffset(board.CurrentMarbleIndex, +2) > board.CurrentMarbleIndex);
            auto inserted = board.PlaceMarble(marble, +2);
            board.MoveCurrentMarbleToPosition(std::get<0>(inserted), std::get<1>(inserted));
        }
        else
        {
            // Player keeps the multiple-of-23 marble, and takes the marble at offset -7, and adds both to their score
            scores[player] += marble;
            scores[player] += board.RemoveMarble(-7);

            // Current marble moves to be one-CW from the removed -7 marble (i.e. offset -6)
            board.MoveCurrentMarble(-6);
        }

        // Test end condition
        if (marble == setup.LastMarble) break;

        // Next player
        player = ((player + 1) % setup.Players);
    }

    // Return final game scores
    std::cout << "\n";
    return scores;
}