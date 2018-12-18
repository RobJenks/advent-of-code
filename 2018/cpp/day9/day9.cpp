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
}

void Day9::Part1(void) const
{
    std::string input = ReadInput(fs::path("day9/input.txt"));
    GameSetup setup = ParseInput(input);

    int result = CalculateHighScore(setup);
    std::cout << "Part 1 result = " << result << "\n";
}

void Day9::RunTests(void) const
{
    std::vector<std::string> input = GetLines(ReadInput(fs::path("day9/tests.txt")));
    for (std::string in : input)
    {
        GameSetup setup = ParseInput(in);
        std::cout << "Test: " << in << "\n";

        int result = CalculateHighScore(setup);
        std::cout << "Result: " << result << "\n\n";
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

int Day9::CalculateHighScore(GameSetup setup) const
{
    Scoreboard scores = PlayGameToMarbleValue(setup);

    int highscore = std::accumulate(scores.begin(), scores.end(), 0, [](int acc, int el) { return std::max(acc, el); });
    return highscore;
}
Day9::Scoreboard Day9::NewScoreboard(int players) const
{
    Scoreboard scoreboard;
    scoreboard.insert(scoreboard.begin(), players, 0);

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
            auto index = board.PlaceMarble(marble, +2);
            board.SetCurrentMarble(index);
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