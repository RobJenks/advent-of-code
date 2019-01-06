#pragma once

#include <string>
#include <sstream>
#include <algorithm>

template <typename T>
class Vec2
{
public:

    T x, y;

    inline Vec2() : x(static_cast<T>(0)), y(static_cast<T>(0)) { }
    inline Vec2(T _xy) : x(_xy), y(_xy) { }
    inline Vec2(T _x, T _y) : x(_x), y(_y) { }

    inline T ManhattanDistance(const Vec2 & other) const { return std::abs(x - other.x) + std::abs(y - other.y); }

    inline bool operator==(const Vec2 & other) const { return (x == other.x && y == other.y); }
    inline bool operator!=(const Vec2 & other) const { return !(*this == other); }

    inline bool operator<(const Vec2 & other) const { return (x < other.x && y < other.y); }
    inline bool operator>=(const Vec2 & other) const { return (x >= other.x && y >= other.y); }

    inline bool operator>(const Vec2 & other) const { return (x > other.x && y > other.y); }
    inline bool operator<=(const Vec2 & other) const { return (x <= other.x && y <= other.y); }

    inline Vec2 & operator+=(const Vec2 & other) { x += other.x; y += other.y; return *this; }
    inline Vec2 & operator-=(const Vec2 & other) { x -= other.x; y -= other.y; return *this; }
    inline Vec2 & operator*=(const Vec2 & other) { x *= other.x; y *= other.y; return *this; }
    inline Vec2 & operator/=(const Vec2 & other) { x /= other.x; y /= other.y; return *this; }

    inline friend Vec2 operator+(Vec2 p0, const Vec2 & p1) { p0 += p1; return p0; }
    inline friend Vec2 operator-(Vec2 p0, const Vec2 & p1) { p0 -= p1; return p0; }
    inline friend Vec2 operator*(Vec2 p0, const Vec2 & p1) { p0 *= p1; return p0; }
    inline friend Vec2 operator/(Vec2 p0, const Vec2 & p1) { p0 /= p1; return p0; }

    static Vec2 Min(const Vec2 & x0, const Vec2 & x1) { return Vec2(std::min(x0.x, x1.x), std::min(x0.y, x1.y)); }
    static Vec2 Max(const Vec2 & x0, const Vec2 & x1) { return Vec2(std::max(x0.x, x1.x), std::max(x0.y, x1.y)); }

    inline static const Vec2 Zero(void) { return Vec2(static_cast<T>(0)); };

    inline std::string str() const
    {
        std::stringstream ss;
        ss << "[" << x << ", " << y << "]";

        return ss.str();
    }
};

template <typename T>
inline std::ostream & operator<<(std::ostream & os, const Vec2<T> & v)
{
    os << v.str();
    return os;
}