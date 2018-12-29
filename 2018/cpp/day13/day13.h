#pragma once

#include "../base/AOCSolution.h"
#include "Tracks.h"

class Day13 : public AOCSolution
{
public:

    // Entry point
    void Run(void) const;


private:

    void RunTests(void) const; 
    void Part1(void) const;

    Tracks BuildTracks(const std::vector<std::string> & input) const;

    bool VerifyTestState(const Tracks & tracks, const std::vector<std::string> & expected) const;

};