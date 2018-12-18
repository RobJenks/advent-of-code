#include "Board.h"
#include <sstream>
#include <algorithm>


Board::Board(void)
{
    NewGame();
}

void Board::NewGame(void)
{
    Data.clear();           // Clear board
    Data.push_back(0);      // Add initial marble
    CurrentMarble = 0;      // Starting point
}

// Place a marble at the given offset from the current marble.  Returns the index of the marble that was placed
int Board::PlaceMarble(int marble, int offset)
{
    auto it = Data.insert(std::next(Data.begin(), IndexOffset(CurrentMarble, offset)), marble);
    return static_cast<int>(std::distance(Data.begin(), it));
}

// Remove a marble at the given offset from the current marble.  Returns the value of 
// the marble that was removed
int Board::RemoveMarble(int offset)
{
    // Identify the marble to be removed
    int index = IndexOffset(CurrentMarble, offset);
    auto it = std::next(Data.begin(), index);
    int value = *it;

    // Remove the marble
    Data.erase(it);
    
    // Adjust the current marble pointer if this would shift it along one
    if (index <= CurrentMarble) --CurrentMarble;

    return value;
}

// Determines the index of the element "offset" elements from "Index".  Offset
// can be positive (for CW traversal) or negative (for CCW traversal)
int Board::IndexOffset(int index, int offset) const
{
    int ix = ((index + offset) % static_cast<int>(Data.size()));
    if (ix < 0) ix += static_cast<int>(Data.size());

    return ix;
}

// Set the current marble to the given board index
void Board::SetCurrentMarble(int index)
{
    CurrentMarble = index;
}

// Move the current marble position by the given offset, positive (CW) or negative (CCW)
void Board::MoveCurrentMarble(int offset)
{
    CurrentMarble = IndexOffset(CurrentMarble, offset);
}

// Debug output
std::string Board::str() const
{
    std::stringstream ss;

    auto it = Data.begin();
    size_t n = std::min(Data.size(), static_cast<size_t>(1000U));

    for (size_t i = 0; i < n; ++i, ++it)
    {
        if (i == CurrentMarble) ss << "(" << *it << ") ";
        else ss << *it << " ";
    }

    if (Data.size() > n) ss << "{...}";

    return ss.str();
}

