#pragma once

#include "../base/AOCSolution.h"
#include <bitset>

class Day3 : public AOCSolution
{
public:

    // Entry point
    void Run(void) const;


private:

    void Part1(void) const;
    void Part2(void) const;

    class Claim
    {
    public:
        int ID;
        int Left, Top, Width, Height;

        Claim() : ID(0), Left(0), Top(0), Width(0), Height(0) { }
    };

    static const int AREA_DIM = 1000;
    static const int AREA_SIZE = (AREA_DIM * AREA_DIM);

    // Transform to linear data for efficiency
    static inline int Index(int x, int y) { return x + (y * AREA_DIM); }

    Claim ParseClaim(const std::string & claim_entry) const;
    void RecordClaim(const Claim & claim, std::bitset<AREA_SIZE> & occupancy, std::bitset<AREA_SIZE> & overlaps) const;
    bool ClaimOverlaps(const Claim & claim, const std::bitset<AREA_SIZE> & overlap_data) const;
};