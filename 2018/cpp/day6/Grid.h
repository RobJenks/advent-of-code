#pragma once

#include <vector>
#include "Point.h"

class Grid
{
public:
    typedef std::vector<Point> GridType;
    typedef GridType::size_type GridIndex;

    GridType Points;
    Point Size;

    inline Grid(Point size)
        :
        Size(size)
    {
        Points.insert(Points.begin(), (size.x * size.y), Point());
    }

    inline GridIndex Index(Point point) const { return Index(point.x, point.y); }
    inline GridIndex Index(int x, int y) const { return static_cast<GridIndex>(x + (y * Size.x)); }
};
