#pragma once

#include <vector>
#include <sstream>
#include "Point.h"

class Grid
{
public:
    typedef std::vector<GridPoint> GridType;
    typedef GridType::size_type GridIndex;

    GridType Points;
    Point Size;

    inline Grid(Point size)
        :
        Size(size)
    {
        for (int i = 0; i < (size.x * size.y); ++i)
        {
            Point location(i % size.x, i / size.x);
            Points.push_back(GridPoint(location));
        }
    }

    inline GridIndex Index(Point point) const { return Index(point.x, point.y); }
    inline GridIndex Index(int x, int y) const { return static_cast<GridIndex>(x + (y * Size.x)); }

    inline bool ValidIndex(int x, int y) const { return (x >= 0 && y >= 0 && x < Size.x && y < Size.y); }

    inline Point ClampToGrid(int x, int y) const { return Point(std::max(std::min(x, Size.x - 1), 0), std::max(std::min(y, Size.y - 1), 0)); }

    inline GridPoint & Get(int x, int y) { return Points[Index(x, y)]; }
    inline const GridPoint & Get(int x, int y) const { return Points[Index(x, y)]; }

    inline std::string str() const
    {
        std::stringstream ss;

        int n = static_cast<int>(Points.size());
        for (int i = 0; i < n; ++i)
        {
            if (i % Size.x == 0) ss << "\n";

            switch (Points[i].ID)
            {
                case GridPoint::AREA_NONE: 
                    ss << "## "; break;
                case GridPoint::AREA_SHARED:
                    ss << ".. "; break;
                default:
                    ss << Points[i].ID << (Points[i].ID < 10 ? "  " : " "); break;
            }
        }
        
        return ss.str();
    }
};
