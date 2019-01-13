#include "Points.h"


Points::Points(const std::vector<Vec4<int>> & points)
    :
    m_constellations(0)
{
    std::transform(points.cbegin(), points.cend(), std::back_inserter(m_points), [](const auto & vec) { return Pt(vec); });
    m_count = static_cast<int>(m_points.size());

    DetermineConstellations();
}


void Points::DetermineConstellations(void)
{
    // Dataset is small enough to just n^2 it
    for (auto & pt : m_points)
    {
        for (int other = 0; other < m_count; ++other)
        {
            auto dist = pt.Position.ManhattanDistance(m_points[other].Position);
            if (dist > 0 && dist <= 3) pt.Links.push_back(other);            
        }
    }

    // Traverse the graph and assign constellation IDs to all connected graphs
    for (auto & pt : m_points)
    {
        if (pt.Constellation == Pt::NO_CONST)
        {
            pt.Constellation = m_constellations++;

            std::vector<int> eval = pt.Links;
            while (!eval.empty())
            {
                auto & other = m_points[eval.back()];
                eval.pop_back();

                if (other.Constellation != Pt::NO_CONST) continue;
                other.Constellation = pt.Constellation;
                eval.insert(eval.end(), other.Links.cbegin(), other.Links.cend());
            }
        }
    }
}