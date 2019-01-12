#pragma once

#include <vector>
#include <cassert>
#include "../common/Vec3.h"


class BotArea
{
public:

    int InRange;
    Vec3<long> Min;
    Vec3<long> Max;
    Vec3<long> Centre;
    long Size;
    long long DistanceToOrigin;

    inline BotArea(void) : BotArea({ 0L,0L,0L }, 0L) { }
    inline BotArea(const Vec3<long> & pmin, long size) 
        : 
        InRange(0), 
        Min(pmin), 
        Size(size)
    {
        Max = (Min + Vec3<long>(size - 1L));     // All node are guaranteed to be cubes
        Centre = (Min + (Size / 2));
        DistanceToOrigin = static_cast<long long>(std::abs(Centre.x) + std::abs(Centre.y) + std::abs(Centre.z));
    }

    inline void Subdivide(std::vector<BotArea> & outVec) const
    {
        assert(!IsUnit());

        auto ctr = Centre;                      
        auto new_size = (Size / 2);             // All nodes will subdivide equally from cubes into cubes

        outVec.emplace_back(BotArea(Min, new_size));                      // --- to ccc
        outVec.emplace_back(BotArea({ ctr.x, Min.y, Min.z }, new_size));  // c-- to +cc
        outVec.emplace_back(BotArea({ Min.x, ctr.y, Min.z }, new_size));  // -c- to c+c
        outVec.emplace_back(BotArea({ ctr.x, ctr.y, Min.z }, new_size));  // cc- to ++c
        outVec.emplace_back(BotArea({ Min.x, Min.y, ctr.z }, new_size));  // --c to cc+
        outVec.emplace_back(BotArea({ ctr.x, Min.y, ctr.z }, new_size));  // c-c to +c+
        outVec.emplace_back(BotArea({ Min.x, ctr.y, ctr.z }, new_size));  // -cc to c++
        outVec.emplace_back(BotArea({ ctr.x, ctr.y, ctr.z }, new_size));  // ccc to +++
    }

    inline bool IsUnit(void) const
    {
        return (Size == 1L);
    }

    inline bool operator<(const BotArea & other) const 
    { 
        if (InRange > other.InRange) return true;
        if ((InRange == other.InRange) && (DistanceToOrigin < other.DistanceToOrigin)) return true;
        if ((InRange == other.InRange) && (DistanceToOrigin == other.DistanceToOrigin) && (Size > other.Size)) return true;

        return false;
    }



};