#pragma once

#include <vector>
#include "../common/Vec4.h"
#include "Pt.h"


class Points
{
public:

    Points(const std::vector<Vec4<int>> & points);

    int GetConstellationCount(void) const { return m_constellations; }

private:

    void DetermineConstellations(void);


private:

    std::vector<Pt> m_points;
    int m_count;
    int m_constellations;

};