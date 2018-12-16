#pragma once

#include <string>
#include <sstream>

class Point
{
public:
    static const int AREA_NONE = -1;
    static const int AREA_SHARED = 0;

    int ID;
    int x, y;

    inline Point() : ID(AREA_NONE), x(0), y(0) { }
    inline Point(int _x, int _y) : ID(AREA_NONE), x(_x), y(_y) { }

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
        ss << "(" << x << ", " << y << ")[" << ID << "]";

        return ss.str();
    }
};

