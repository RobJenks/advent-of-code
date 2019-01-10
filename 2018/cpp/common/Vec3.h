#pragma once

#include <string>
#include <sstream>
#include <algorithm>

template <typename T>
class Vec3
{
public:

    T x, y, z;

    inline Vec3() : x(static_cast<T>(0)), y(static_cast<T>(0)), z(static_cast<T>(0)) { }
    inline Vec3(T _xyz) : x(_xyz), y(_xyz), z(_xyz) { }
    inline Vec3(T _x, T _y, T _z) : x(_x), y(_y), z(_z) { }

    inline T ManhattanDistance(const Vec3 & other) const { return std::abs(x - other.x) + std::abs(y - other.y) + std::abs(z - other.z); }

    inline bool operator==(const Vec3 & other) const { return (x == other.x && y == other.y && z == other.z); }
    inline bool operator!=(const Vec3 & other) const { return !(*this == other); }

    inline bool operator<(const Vec3 & other) const { return (x < other.x && y < other.y && z < other.z); }
    inline bool operator>=(const Vec3 & other) const { return (x >= other.x && y >= other.y && z >= other.z); }

    inline bool operator>(const Vec3 & other) const { return (x > other.x && y > other.y && z > other.z); }
    inline bool operator<=(const Vec3 & other) const { return (x <= other.x && y <= other.y && z <= other.z); }

    inline Vec3 & operator+=(const Vec3 & other) { x += other.x; y += other.y; z += other.z; return *this; }
    inline Vec3 & operator-=(const Vec3 & other) { x -= other.x; y -= other.y; z -= other.z;; return *this; }
    inline Vec3 & operator*=(const Vec3 & other) { x *= other.x; y *= other.y; z *= other.z;; return *this; }
    inline Vec3 & operator/=(const Vec3 & other) { x /= other.x; y /= other.y; z /= other.z;; return *this; }

    inline friend Vec3 operator+(Vec3 p0, const Vec3 & p1) { p0 += p1; return p0; }
    inline friend Vec3 operator-(Vec3 p0, const Vec3 & p1) { p0 -= p1; return p0; }
    inline friend Vec3 operator*(Vec3 p0, const Vec3 & p1) { p0 *= p1; return p0; }
    inline friend Vec3 operator/(Vec3 p0, const Vec3 & p1) { p0 /= p1; return p0; }

    static Vec3 Min(const Vec3 & x0, const Vec3 & x1) { return Vec3(std::min(x0.x, x1.x), std::min(x0.y, x1.y), std::min(x0.z, x1.z)); }
    static Vec3 Max(const Vec3 & x0, const Vec3 & x1) { return Vec3(std::max(x0.x, x1.x), std::max(x0.y, x1.y), std::max(x0.z, x1.z)); }

    inline static const Vec3 Zero(void) { return Vec3(static_cast<T>(0)); };

    inline std::string str() const
    {
        std::stringstream ss;
        ss << "[" << x << ", " << y << ", " << z << "]";

        return ss.str();
    }
};

template <typename T>
inline std::ostream & operator<<(std::ostream & os, const Vec3<T> & v)
{
    os << v.str();
    return os;
}