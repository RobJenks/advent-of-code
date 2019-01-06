#include "Forest.h"
#include <cassert>
#include "../common/StringUtil.h"


Forest::Forest(const Vec2<int> & size)
    :
    m_size(size), 
    m_count(size.x * size.y)
{
    m_data.insert(m_data.begin(), m_count, CellState::None);
    m_transformed.insert(m_transformed.begin(), m_count, CellState::None);
}

Forest::Forest(const Vec2<int> & size, TData data)
    :
    m_size(size),
    m_count(size.x * size.y),
    m_data(data)
{
    m_transformed.insert(m_transformed.begin(), m_count, CellState::None);
}


void Forest::Populate(const std::vector<std::string> & input)
{
    auto it = m_data.begin();
    for (int y = 0; y < input.size(); ++y)
    {
        const auto & line = StringUtil::Trim(input[y]);
        for (int x = 0; x < line.size(); ++x)
        {
            *(it++) = Forest::GetSchematicState(line[x]);
        }
    }
}

void Forest::Evaluate(const size_t iterations)
{
    for (size_t i = 0U; i < iterations; ++i)
    {
        Evaluate();
    }
}

void Forest::Evaluate(void)
{
    auto it = m_transformed.begin();
    for (size_t i = 0U; i < m_count; ++i)
    {
        *(it++) = Transform(i);
    }

    m_data = m_transformed;
}


Forest::CellState Forest::Transform(size_t index) const
{
    std::array<CellState, 8U> adj;
    
    auto coord = Coord(index);
    if (coord.x == 0 || coord.y == 0 || coord.x == (m_size.x - 1) || coord.y == (m_size.y - 1))
    {
        GetAdjacent(index, adj);
    }
    else
    {
        GetAdjacentInner(index, adj);
    }

    int count = 0;
    auto state = m_data[index];
    if (state == CellState::Open)
    {
        // Open -> (3 adj trees) -> Trees
        for (size_t i = 0U; i < 8U; ++i) if (adj[i] == CellState::Trees && ++count == 3) return CellState::Trees;
        return CellState::Open;
    }
    else if (state == CellState::Trees)
    {
        // Trees -> (3 adj lumberyards) -> Lumberyard
        for (size_t i = 0U; i < 8U; ++i) if (adj[i] == CellState::Lumberyard && ++count == 3) return CellState::Lumberyard;
        return CellState::Trees;
    }
    else if (state == CellState::Lumberyard)
    {
        // Lumberyard -> (1 adj lumberyard, 1 adj trees) -> stays Lumberyard, else Open
        for (size_t i = 0U; i < 8U; ++i)
        {
            if (adj[i] == CellState::Lumberyard && ((count |= 1) == (1 | 2))) return CellState::Lumberyard;
            else if (adj[i] == CellState::Trees && ((count |= 2) == (1 | 2))) return CellState::Lumberyard;
        }
        return CellState::Open;
    }

    assert(false);
    return CellState::None;
}

// Return the state of all surrounding elements when the cell if known to NOT be an edge.  Can be applied
// to the majority of cells and avoids bounds-checking each lookup
void Forest::GetAdjacentInner(size_t index, std::array<CellState, 8U>(&state)) const
{
    --index; 
    state[0] = m_data[index];      // Left

    index -= m_size.x;
    state[1] = m_data[index];      // Left-up

    ++index;
    state[2] = m_data[index];      // Up

    ++index;
    state[3] = m_data[index];      // Right-up

    index += m_size.x;
    state[4] = m_data[index];      // Right

    index += m_size.x;
    state[5] = m_data[index];      // Down-right

    --index;
    state[6] = m_data[index];      // Down

    --index;
    state[7] = m_data[index];      // Down-left
}

// Return the state of all adjacent cells, for a cell that may or may not be an edge cell
void Forest::GetAdjacent(size_t index, std::array<CellState, 8U>(&state)) const
{
    state = { CellState::None };

    auto up = GetUp(index);
    if (up != NO_CELL)
    {
        state[0] = m_data[up];
        state[1] = GetState(GetLeft(up));
        state[2] = GetState(GetRight(up));
    }

    auto down = GetDown(index);
    if (down != NO_CELL)
    {
        state[3] = m_data[down];
        state[4] = GetState(GetLeft(down));
        state[5] = GetState(GetRight(down));
    }

    state[6] = GetState(GetLeft(index));
    state[7] = GetState(GetRight(index));
}


size_t Forest::GetLeft(size_t index) const
{
    return (index % m_size.x != 0 ? (index - 1) : NO_CELL);
}

size_t Forest::GetRight(size_t index) const
{
    auto right = (index + 1);
    return (right % m_size.x != 0 ? right : NO_CELL);
}

size_t Forest::GetUp(size_t index) const
{
    return (index >= m_size.x ? (index - m_size.x) : NO_CELL);
}

size_t Forest::GetDown(size_t index) const
{
    auto down = (index + m_size.x);
    return (down < m_count ? down : NO_CELL);
}

Forest::CellState Forest::GetState(size_t index) const
{
    return (index == NO_CELL ? CellState::None : m_data[index]);
}

std::array<int, Forest::STATE_COUNT> Forest::GetStateCounts(void) const
{
    std::array<int, STATE_COUNT> states = { 0 };
    std::for_each(m_data.cbegin(), m_data.cend(), [&states](const CellState state) { ++states[static_cast<int>(state)]; });

    return states;
}

int Forest::GetResourceValue(void) const
{
    auto states = GetStateCounts();
    return (states[static_cast<int>(CellState::Trees)] * states[static_cast<int>(CellState::Lumberyard)]);
}


char Forest::GetSchematic(CellState state)
{
    switch (state)
    {
        case CellState::Open:           return '.';
        case CellState::Trees:          return '|';
        case CellState::Lumberyard:     return '#';

        default:
            assert(false);
            return 'X';
    }
}

Forest::CellState Forest::GetSchematicState(char c)
{
    switch (c)
    {
        case '.':       return CellState::Open;
        case '|':       return CellState::Trees;
        case '#':       return CellState::Lumberyard;

        default:
            assert(false);
            return CellState::Open;
    }
}

std::string Forest::str(void) const
{
    std::stringstream ss;

    int x = 0;
    for (auto cell : m_data)
    {
        ss << GetSchematic(cell);
        if (++x == m_size.x) { ss << '\n'; x = 0; }
    }

    return ss.str();
}