#pragma once

#include <vector>
#include <algorithm>
#include "../common/Vec2.h"
class Tile;


// Performs reading-order prioritisation for relevant types
class RO
{
public:

    static Vec2<int> Coords(const Vec2<int> & v0, const Vec2<int> & v1, const int x_bound);

    // Requires T::operator< implementation in reading-order
    template <typename T>
    static T & Objects(T & t0, T & t1);

    // Requires T::operator< implementation in reading-order
    template <typename T>
    static T ObjVal(T t0, T t1);

    // Requires T::operator< implementation in reading-order
    template <typename T>
    static void VectorInPlace(std::vector<T> & v);

    // Requires T::operator< implementation in reading-order
    template <typename T>
    static std::vector<T> Vector(const std::vector<T> & v);

    

};


template <typename T>
T & RO::Objects(T & t0, T & t1)
{
    return (t0 < t1 ? t0 : t1);
}

template <typename T>
T RO::ObjVal(T t0, T t1)
{
    return (t0 < t1 ? t0 : t1);
}

template <typename T>
void RO::VectorInPlace(std::vector<T> & v)
{
    std::sort(v.begin(), v.end());
}

template <typename T>
std::vector<T> RO::Vector(const std::vector<T> & v)
{
    std::vector<T> sorted(v.begin(), v.end());
    VectorInPlace(sorted);

    return sorted;
}