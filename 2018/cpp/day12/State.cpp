#include "State.h"
#include <numeric>
#include <cassert>

// Default constructor
State::State(void) 
    : 
    m_zeropoint(0U) 
{ 
}

// Construct from a given state string
State::State(const std::string & state)
    :
    m_zeropoint(0U)
{
    AddNegativeBuffer(5U);        // Start with a buffer for leftward movement

    std::for_each(state.begin(), state.end(), [this](char c) {
        if (c == '#') m_data.push_back(true);
        else if (c == '.') m_data.push_back(false);
    });
}

// Construct from a subset of a given state object
State::State(const TData::const_iterator it_start, const TData::const_iterator it_end, size_t zero_point)
    :
    m_zeropoint(zero_point),
    m_data(it_start, it_end)
{
}

// Output current state
std::string State::str(bool zero_base) const
{
    std::stringstream ss;
    std::for_each(m_data.begin() + (zero_base ? m_zeropoint : 0U), m_data.end(), [&ss](bool b) { ss << (b ? '#' : '.'); });

    return ss.str();
}

// Prepend a negative buffer to account for leftward movement
void State::AddNegativeBuffer(size_t size)
{
    m_data.insert(m_data.begin(), size, false);
    m_zeropoint += size;
}

// Append a positive buffer to account for leftward movement
void State::AddPositiveBuffer(size_t size)
{
    m_data.insert(m_data.end(), size, false);
}

// Apply all given rules to all elements of the current state simultaneously
void State::ApplyRules(const std::vector<Rule> & rules)
{
    // Ensure we always have an empty negative and positive buffer
    if (!Equal(m_data.begin() + 2, { false, false, false, false, false })) AddNegativeBuffer(5U);
    if (!Equal(m_data.end() - 3, { false, false, false, false, false })) AddPositiveBuffer(5U);

    // Process every cell simultaneously and transform based on rules where applicable
    TData prev = m_data;
    TData::iterator dest = m_data.begin()+2;
    for (TData::const_iterator src = prev.begin()+2; src != prev.end()-2; ++src, ++dest)
    {
        // Only positive rules are stored, so value is simply determined by whether we find a matching rule condition
        *dest = std::find_if(rules.begin(), rules.end(), [this, src](const auto & rule) {
            return Equal(src, rule.Condition);
        }) != rules.end();
    }
}

Value State::Equal(TData::const_iterator centre, const std::array<Value, 5U> & comp)
{
    return (*(centre - 2) == comp[0] && *(centre - 1) == comp[1] && *centre == comp[2]
        &&  *(centre + 1) == comp[3] && *(centre + 2) == comp[4]);
}

int State::DetermineStateScore(void) const
{
    int index = (0 - static_cast<int>(m_zeropoint)) - 1;
    return std::accumulate(m_data.begin(), m_data.end(), 0, [&index](int acc, Value el) {
        ++index;
        if (el) return acc + index;
        else    return acc;
    });
}

std::string State::GetActivePattern(void) const
{
    auto it = std::find(m_data.begin(), m_data.end(), Value::True);

    auto it_end = m_data.end()-1;
    while (*it_end == Value::False) --it_end;

    std::string pattern;
    std::for_each(it, it_end, [&pattern](Value el) { pattern.push_back(el ? '#' : '.'); });
    return pattern;
}