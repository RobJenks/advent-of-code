#pragma once

#include "../common/Vec3.h"

class Bot
{
public:

    int ID;
    Vec3<long> Position;
    long Radius;


public:

    Bot(void) : ID(-1), Position(0L), Radius(0L) { }
    Bot(int id, Vec3<long> position, long radius);


    std::string str(void) const;

private:



};

inline std::ostream & operator<<(std::ostream & os, const Bot & bot)
{
    os << bot.str();
    return os;
}