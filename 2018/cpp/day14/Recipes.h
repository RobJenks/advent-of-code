#pragma once

#include <vector>
#include <array>
#include <sstream>


template <unsigned int WORKERS>
class Recipes
{
public:

    typedef int Score;
    typedef size_t Recipe;

    Recipes(std::vector<Score> initial_state);
     
    void EvaluateIterations(const size_t iterations);
    void EvaluateToRecipeCount(const size_t recipe_count);
    size_t EvaluateToTerminatingSequence(const std::string & sequence);
    
    std::string str(void) const;
    std::string str(const size_t begin, const size_t count) const;
    std::string str_compact(void) const;
    std::string str_compact(const size_t begin, const size_t count) const;

    

private:

    std::vector<Score> m_data;
    std::array<Recipe, WORKERS> m_workers;

    inline static const std::array<std::pair<char, char>, WORKERS> MARKERS = { std::pair<char,char>{ '(',')' }, std::pair<char,char>{ '[',']' } };

    void Evaluate(void);


};

// Construct for a given initial state
template <unsigned int WORKERS>
Recipes<WORKERS>::Recipes(std::vector<Score> initial_state)
    :
    m_data(initial_state),
    m_workers{ 0 }
{
    for (int i = 0; i < WORKERS; ++i) m_workers[i] = i;
}

// Evaluate for the given number of iterations
template <unsigned int WORKERS>
void Recipes<WORKERS>::EvaluateIterations(const size_t iterations)
{
    for (size_t i = 0U; i < iterations; ++i)
    {
        Evaluate();
    }
}

// Evaluate up to the given recipe count
template <unsigned int WORKERS>
void Recipes<WORKERS>::EvaluateToRecipeCount(const size_t recipe_count)
{
    while (m_data.size() < recipe_count)
    {
        Evaluate();
    }
}

// Evaluate until the score vector ends with the given sequence.  Returns the 
// number of recipes in the vector prior to this sequence
template <unsigned int WORKERS>
size_t Recipes<WORKERS>::EvaluateToTerminatingSequence(const std::string & sequence)
{
    std::vector<int> terminator;
    std::transform(sequence.begin(), sequence.end(), std::back_inserter(terminator), [](char c) { return (c - '0'); });

    // Must evaluate to at least the length of the terminator
    const size_t termsize = terminator.size();
    EvaluateToRecipeCount(termsize);

    size_t index;
    while (true)
    {
        const auto prior_size = m_data.size();

        Evaluate();

        const auto count = m_data.size();
        const auto diff = (count - prior_size);

        // Need to account for potentially multiple new elements -> potentially multiple new matching sequences
        for (size_t offset = 0U; offset < diff; ++offset)
        {
            const auto seq_start = (count - termsize) - offset;
            for (index = 0U; index < termsize; ++index)
            {
                if (m_data[seq_start + index] != terminator[index]) break;
            }

            // This will only be true if all components of the terminator were matched
            if (index == termsize)
            {
                return seq_start;
            }
        }
    }
}

// Evaluate and generate a new state
template <unsigned int WORKERS>
void Recipes<WORKERS>::Evaluate(void)
{
    int r0 = m_data[m_workers[0]];
    int r1 = m_data[m_workers[1]];
    int sum = r0 + r1;

    if (sum < 10)
    {
        m_data.push_back(sum);
    }
    else
    {
        m_data.push_back((sum / 10) % 10);
        m_data.push_back(sum % 10);
    }

    size_t count = m_data.size();
    m_workers[0] = (m_workers[0] + 1U + r0) % count;
    m_workers[1] = (m_workers[1] + 1U + r1) % count;
}



// Output a string representation of the current state
template <unsigned int WORKERS>
std::string Recipes<WORKERS>::str(void) const
{
    return str(0U, m_data.size());
}

// Output a string representation of the current state, for the given range of cells
template <unsigned int WORKERS>
std::string Recipes<WORKERS>::str(const size_t begin, const size_t count) const
{
    std::stringstream ss;

    for (size_t i = begin; i < count; ++i)
    {
        bool marked = false;
        for (size_t w = 0; w < WORKERS; ++w)
        {
            if (m_workers[w] == i)
            {
                ss << MARKERS[w].first << m_data[i] << MARKERS[w].second;
                marked = true;
                break;
            }
        }

        if (!marked) ss << ' ' << m_data[i] << ' ';
    }

    return ss.str();
}

// Output a compacted string representation of the current state
template <unsigned int WORKERS>
std::string Recipes<WORKERS>::str_compact(void) const
{
    return str_compact(0U, m_data.size());
}

// Output a compacted string representation of the current state, for the given range of cells
template <unsigned int WORKERS>
std::string Recipes<WORKERS>::str_compact(const size_t begin, const size_t count) const
{
    std::stringstream ss;

    auto it_end = (m_data.cbegin() + begin + count);
    for (auto it = m_data.cbegin() + begin; it != it_end; ++it)
    {
        ss << *it;
    }

    return ss.str();
}
