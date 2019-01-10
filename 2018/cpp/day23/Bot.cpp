#include "Bot.h"
#include <sstream>


Bot::Bot(int id, Vec3<long> position, long radius)
    :
    ID(id),
    Position(position),
    Radius(radius)
{
}




std::string Bot::str(void) const
{
    std::stringstream ss;

    ss << "{ ID=" << ID << ", Pos=" << Position << ", Radius=" << Radius << " }";
    return ss.str();
}
