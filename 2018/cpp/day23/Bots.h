#pragma once

#include <vector>
#include <string>
#include "../common/Vec3.h"
#include "Bot.h"
class BotArea;

class Bots
{
public:

    inline Bots(void) { }

    inline const Bot & Get(int id) const { return m_data[id]; }
    inline const std::vector<Bot> & GetAll(void) const { return m_data; }

    int Add(Vec3<long> position, long radius);
    size_t Count(void) const { return m_data.size(); }

    int GetStrongest(void) const;

    int GetBotsInRange(int id) const;
    int GetBotsInRange(const Vec3<long> coord) const;
    int GetBotsInRange(const Vec3<long> pmin, const Vec3<long> pmax) const;

private:

    std::vector<Bot> m_data;


};