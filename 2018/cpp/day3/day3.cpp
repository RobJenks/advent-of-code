#include "day3.h"
#include <iostream>
#include <sstream>
#include <algorithm>

void Day3::Run(void) const
{
    std::cout << "\nDay 3:\n";

    Part1();
    Part2();
}

void Day3::Part1(void) const
{
    std::vector<std::string> input = GetLines(ReadInput(fs::path("day3/input.txt")));

    std::vector<Claim> claims;
    std::transform(input.begin(), input.end(), std::back_inserter(claims), 
        [this](const std::string & el) { return this->ParseClaim(el); });

    // Straightforward but (fairly) memory-wasteful solution.  Bitset is packed at 1 bit per element though 
    // so still reasonable.  More space-efficient than storing any numeric data (min 8 bits /el)
    std::bitset<AREA_SIZE> occupied = { 0 };
    std::bitset<AREA_SIZE> overlapping = { 0 };

    // Record claims and determine overlap
    std::for_each(claims.begin(), claims.end(), [this, &occupied, &overlapping](const auto & el) { RecordClaim(el, occupied, overlapping); });

    // No STL iterator support for bitset
    int result = 0;
    for (int i = 0; i < AREA_SIZE; ++i) { result += (overlapping[i] ? 1 : 0); }
    
    std::cout << "Part 1 result = " << result << "\n";
}

void Day3::Part2(void) const
{
    std::vector<std::string> input = GetLines(ReadInput(fs::path("day3/input.txt")));

    std::vector<Claim> claims;
    std::transform(input.begin(), input.end(), std::back_inserter(claims),
        [this](const std::string & el) { return this->ParseClaim(el); });

    // Record claims and determine overlap
    std::bitset<AREA_SIZE> occupied = { 0 };
    std::bitset<AREA_SIZE> overlapping = { 0 };
    std::for_each(claims.begin(), claims.end(), [this, &occupied, &overlapping](const auto & el) { RecordClaim(el, occupied, overlapping); });

    // Guaranteed exactly one match
    int result = std::find_if(claims.begin(), claims.end(), [this, &overlapping](const auto & el) 
        { return !this->ClaimOverlaps(el, overlapping); }
    )->ID;

    std::cout << "Part 2 result = " << result << "\n";
}

Day3::Claim Day3::ParseClaim(const std::string & claim_entry) const
{
    Claim claim;
    char sink;
    std::stringstream ss(claim_entry.substr(1U));   // Skip leading #

    ss >> claim.ID >> sink >> claim.Left >> sink >> claim.Top >> sink >> claim.Width >> sink >> claim.Height;

    return claim;
}

void Day3::RecordClaim(const Claim & claim, std::bitset<AREA_SIZE> & occupancy, std::bitset<AREA_SIZE> & overlaps) const
{
    for (int x = claim.Left; x < (claim.Left + claim.Width); ++x)
    {
        for (int y = claim.Top; y < (claim.Top + claim.Height); ++y)
        {
            int index = Index(x, y);
            
            if (occupancy[index]) overlaps[index] = true;
            occupancy[index] = true;
        }
    }
}

bool Day3::ClaimOverlaps(const Claim & claim, const std::bitset<AREA_SIZE> & overlap_data) const
{
    for (int x = claim.Left; x < (claim.Left + claim.Width); ++x)
    {
        for (int y = claim.Top; y < (claim.Top + claim.Height); ++y)
        {
            if (overlap_data[Index(x, y)]) return true;
        }
    }

    return false;
}