#pragma once

#include "../base/AOCSolution.h"


class Day9 : public AOCSolution
{
public:

    // Entry point
    void Run(void) const;


private:

    typedef unsigned long long Score;
    typedef std::vector<Score> Scoreboard;


    struct GameSetup
    {
        int Players;
        int LastMarble;

        GameSetup(int players, int last_marble) : Players(players), LastMarble(last_marble) { }
    };

private:

    void Part1(void) const;
    void Part2(void) const;
    void RunTests(void) const;

    GameSetup ParseInput(const std::string & input) const;
    Scoreboard NewScoreboard(int players) const;

    Score CalculateHighScore(GameSetup setup) const;
    Scoreboard PlayGameToMarbleValue(GameSetup setup) const;
};