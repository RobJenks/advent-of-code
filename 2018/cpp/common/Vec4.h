#pragma once

#include <string>
#include <sstream>
#include <algorithm>

template <typename T>
class Vec4
{
public:

    T x, y, z, w;

    inline Vec4(void) : Vec4(static_cast<T>(0)) { }
    inline Vec4(T _xyzw) : x(_xyzw), y(_xyzw), z(_xyzw), w(_xyzw) { }
    inline Vec4(T _x, T _y, T _z, T _w) : x(_x), y(_y), z(_z), w(_w) { }

    inline T ManhattanDistance(const Vec4 & other) const { 
        return std::abs(x - other.x) + std::abs(y - other.y) + std::abs(z - other.z) + std::abs(w - other.w); 
    }

    inline bool operator==(const Vec4 & other) const { return (x == other.x && y == other.y && z == other.z && w == other.w); }
    inline bool operator!=(const Vec4 & other) const { return !(*this == other); }

    inline bool operator<(const Vec4 & other) const { return (x < other.x && y < other.y && z < other.z && w < other.w); }
    inline bool operator>=(const Vec4 & other) const { return (x >= other.x && y >= other.y && z >= other.z && w >= other.w); }

    inline bool operator>(const Vec4 & other) const { return (x > other.x && y > other.y && z > other.z && w > other.w); }
    inline bool operator<=(const Vec4 & other) const { return (x <= other.x && y <= other.y && z <= other.z && w < other.w); }

    inline Vec4 & operator+=(const Vec4 & other) { x += other.x; y += other.y; z += other.z; w += other.w; return *this; }
    inline Vec4 & operator-=(const Vec4 & other) { x -= other.x; y -= other.y; z -= other.z; w -= other.w; return *this; }
    inline Vec4 & operator*=(const Vec4 & other) { x *= other.x; y *= other.y; z *= other.z; w *= other.w; return *this; }
    inline Vec4 & operator/=(const Vec4 & other) { x /= other.x; y /= other.y; z /= other.z; w /= other.w; return *this; }

    inline friend Vec4 operator+(Vec4 p0, const Vec4 & p1) { p0 += p1; return p0; }
    inline friend Vec4 operator-(Vec4 p0, const Vec4 & p1) { p0 -= p1; return p0; }
    inline friend Vec4 operator*(Vec4 p0, const Vec4 & p1) { p0 *= p1; return p0; }
    inline friend Vec4 operator/(Vec4 p0, const Vec4 & p1) { p0 /= p1; return p0; }

    static Vec4 Min(const Vec4 & x0, const Vec4 & x1) { return Vec4(std::min(x0.x, x1.x), std::min(x0.y, x1.y), std::min(x0.z, x1.z), std::min(x0.w, x1.w)); }
    static Vec4 Max(const Vec4 & x0, const Vec4 & x1) { return Vec4(std::max(x0.x, x1.x), std::max(x0.y, x1.y), std::max(x0.z, x1.z), std::max(x0.w, x1.w)); }

    inline static const Vec4 Zero(void) { return Vec4(static_cast<T>(0)); };

    inline std::string str() const
    {
        std::stringstream ss;
        ss << "[" << x << ", " << y << ", " << z << ", " << w << "]";

        return ss.str();
    }
};

template <typename T>
inline std::ostream & operator<<(std::ostream & os, const Vec4<T> & v)
{
    os << v.str();
    return os;
}