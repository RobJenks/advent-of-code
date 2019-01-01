#include "RO.h"


Vec2<int> RO::Coords(const Vec2<int> & v0, const Vec2<int> & v1, const int x_bound)
{
    return ((v0.x + (v0.y * x_bound)) < (v1.x + (v1.y * x_bound)) ? v0 : v1);
}
