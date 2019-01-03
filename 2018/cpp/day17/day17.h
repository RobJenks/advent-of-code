#pragma once

#include "../base/AOCSolution.h"
#include "Terrain.h"

class Day17 : public AOCSolution
{
public:

    // Entry point
    void Run(void) const;



private:

    void RunTests(void) const; 

    void Part1(void) const;

    
    Terrain::TerrainInputCollection ParseTerrainInput(const std::vector<std::string> & input) const;
};