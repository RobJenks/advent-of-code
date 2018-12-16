#pragma once

#include <string>
#include <sstream>

class Point
{
public:
    
    int x, y;

    inline Point() : x(0), y(0) { }
    inline Point(int _x, int _y) : x(_x), y(_y) { }

    inline int ManhattanDistance(const Point & other) { return std::abs(x - other.x) + std::abs(y - other.y); }

    inline bool operator==(const Point & other) const { return (x == other.x && y == other.y); }
    inline bool operator!=(const Point & other) const { return !(*this == other); }
    
    inline bool operator<(const Point & other) const { return (x < other.x && y < other.y); }
    inline bool operator>=(const Point & other) const { return !(*this < other); }

    inline bool operator>(const Point & other) const { return (x > other.x && y > other.y); }
    inline bool operator<=(const Point & other) const { return !(*this > other); }

    inline Point & operator+=(const Point & other) { x += other.x; y += other.y; return *this; }
    inline Point & operator-=(const Point & other) { x -= other.x; y -= other.y; return *this; }
    inline Point & operator*=(const Point & other) { x *= other.x; y *= other.y; return *this; }
    inline Point & operator/=(const Point & other) { x /= other.x; y /= other.y; return *this; }

    inline friend Point operator+(Point p0, const Point & p1) { p0 += p1; return p0; }
    inline friend Point operator-(Point p0, const Point & p1) { p0 -= p1; return p0; }
    inline friend Point operator*(Point p0, const Point & p1) { p0 *= p1; return p0; }
    inline friend Point operator/(Point p0, const Point & p1) { p0 /= p1; return p0; }

    inline std::string str() const
    {
        std::stringstream ss;
        ss << "(" << x << ", " << y << ")";

        return ss.str();
    }
};

class GridPoint
{
public:

    static const int AREA_NONE = -1;
    static const int AREA_SHARED = 0;

    Point Location;
    int ID;
    int Distance;

    inline GridPoint(void) : GridPoint(Point(0, 0)) { }
    inline GridPoint(Point location) : Location(location), ID(AREA_NONE), Distance(0) { }
};