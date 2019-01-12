#include "Bot.h"
#include <sstream>


Bot::Bot(int id, Vec3<long> position, long radius)
    :
    ID(id),
    Position(position),
    Radius(radius)
{
}


bool Bot::InRange(const Vec3<long> & pmin, const Vec3<long> & pmax) const
{
    long dist_outside = 0;

    if (Position.x > pmax.x) dist_outside += std::abs(Position.x - pmax.x);
    if (Position.x < pmin.x) dist_outside += std::abs(pmin.x - Position.x);
    if (Position.y > pmax.y) dist_outside += std::abs(Position.y - pmax.y);
    if (Position.y < pmin.y) dist_outside += std::abs(pmin.y - Position.y);
    if (Position.z > pmax.z) dist_outside += std::abs(Position.z - pmax.z);
    if (Position.z < pmin.z) dist_outside += std::abs(pmin.z - Position.z);

    return dist_outside <= Radius;
}


std::string Bot::str(void) const
{
    std::stringstream ss;

    ss << "{ ID=" << ID << ", Pos=" << Position << ", Radius=" << Radius << " }";
    return ss.str();
}
