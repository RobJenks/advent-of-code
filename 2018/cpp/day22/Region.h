#pragma once

class Region
{
public:

    enum class RegionType { Rocky = 0, Wet, Narrow, Unknown };

public:

    long GeologicIndex;
    int ErosionLevel;
    RegionType Type;

public:

    inline Region(void) : GeologicIndex(0L), ErosionLevel(0), Type(RegionType::Unknown) { }
    inline Region(long geo_index, int erosion_level, RegionType type) : GeologicIndex(geo_index), ErosionLevel(erosion_level), Type(type) { }
  
};