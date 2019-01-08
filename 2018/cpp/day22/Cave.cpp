#include "Cave.h"
#include <numeric>

const std::array<Region::RegionType, 3U> Cave::REGION_EROSION = { static_cast<Region::RegionType>(0), static_cast<Region::RegionType>(1),static_cast<Region::RegionType>(2) };
const std::array<char, 3U> Cave::REGION_SCHEMATIC = { '.', '=', '|' };

Cave::Cave(int depth, Vec2<int> target)
    :
    m_depth(depth),
    m_target(target)
{
    // We only need to calculate the region data up to and including the target position
    m_size = m_target + Vec2<int>(1, 1);
    m_count = (m_size.x * m_size.y);
    m_data.insert(m_data.begin(), m_count, Region());

    // Calculate start and end points directly since they are special cases with geo index == 0
    m_data[0] = m_data[m_count-1U] = CalculateRegion(0L);

    // Build the region data
    long geo = 0L;
    Vec2<int> pos = { 1, 0 };
    for (size_t i = 1U; i < m_count-1U; ++i)
    {
        if (pos.y == 0) geo = (pos.x * 16807);
        else if (pos.x == 0) geo = (pos.y * 48271);
        else geo = (m_data[i - 1].ErosionLevel * m_data[i - m_size.x].ErosionLevel);    // [x-1][y] * [x][y-1]

        m_data[i].GeologicIndex = geo;
        m_data[i].ErosionLevel = DetermineErosion(geo);
        m_data[i].Type = DetermineRegionType(m_data[i].ErosionLevel);

        if (++pos.x == m_size.x) { pos.x = 0; ++pos.y; }
    }
}


Region Cave::CalculateRegion(long geo_index) const 
{
    Region region;
    region.GeologicIndex = geo_index;
    region.ErosionLevel = DetermineErosion(geo_index);
    region.Type = DetermineRegionType(region.ErosionLevel);

    return region;
}

int Cave::CalculateRiskScore(void) const
{
    // Risk: Rocky = 1, Wet = 2, Narrow = 3 --> same as the underlying int values, so use them
    return std::accumulate(m_data.begin(), m_data.end(), 0, [](int acc, const Region & el) { 
        return (acc + static_cast<int>(el.Type)); 
    });
}

size_t Cave::GetLeft(size_t index) const
{
    return (index % m_size.x != 0 ? (index - 1) : NO_CELL);
}

size_t Cave::GetRight(size_t index) const
{
    auto right = (index + 1);
    return (right % m_size.x != 0 ? right : NO_CELL);
}

size_t Cave::GetUp(size_t index) const
{
    return (index >= m_size.x ? (index - m_size.x) : NO_CELL);
}

size_t Cave::GetDown(size_t index) const
{
    auto down = (index + m_size.x);
    return (down < m_count ? down : NO_CELL);
}

std::string Cave::str(void) const
{
    std::stringstream ss;

    ss << 'M';
    int x = 1;
    for (size_t i = 1U; i < m_count - 1; ++i)
    {
        ss << REGION_SCHEMATIC[static_cast<int>(m_data[i].Type)];
        if (++x == m_size.x) { ss << '\n'; x = 0; }
    }
    ss << "T\n";

    return ss.str();
}