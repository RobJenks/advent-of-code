#include "Bots.h"
#include <algorithm>
#include <numeric>



int Bots::Add(Vec3<long> position, long radius)
{
    int id = static_cast<int>(m_data.size());
    m_data.push_back(Bot(id, position, radius));

    return id;
}



// Returns the ID of the strongest bot, i.e. the bot with the largest signal radius
int Bots::GetStrongest(void) const
{
    long rad = -1L;
    return std::accumulate(m_data.cbegin(), m_data.cend(), -1, [&rad](int acc, const Bot & bot) {
        if (bot.Radius > rad) { rad = bot.Radius; return bot.ID; }
        return acc;
    });
}


// Returns all bots in range of the specified bot.  Performs simple linear traversal since this is
// a single query.  Will require extension to e.g. octree spatial partitioning for more extensive queries
int Bots::GetBotsInRange(int id) const
{
    const Vec3<long> pos = m_data[id].Position;
    const long range = m_data[id].Radius;

    return std::count_if(m_data.begin(), m_data.end(), [pos, range](const Bot & bot) {
        return (bot.Position.ManhattanDistance(pos) <= range);
    });
}
