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
    CurrentMarble = Data.begin();   // Starting point
    CurrentMarbleIndex = 0;         // Starting point
}

// Place a marble at the given offset from the current marble.  Returns an iterator to the marble that was placed
std::tuple<Board::TData::iterator, bool> Board::PlaceMarble(int marble, int offset)
{
    auto move = IndexOffset(CurrentMarbleIndex, offset) - CurrentMarbleIndex;
    auto insert_point = GetOffsetFromCurrentMarble(move);
    bool inserting_after_current = (move > 0);

    auto it = Data.insert(insert_point, marble);
    if (!inserting_after_current) ++CurrentMarbleIndex;

    return { it, inserting_after_current };
}

// Remove a marble at the given offset from the current marble.  Returns the value of 
// the marble that was removed
int Board::RemoveMarble(int offset)
{
    // Identify the marble to be removed
    auto index = IndexOffset(CurrentMarbleIndex, offset);
    auto move = (index - CurrentMarbleIndex);
    auto remove_point = GetOffsetFromCurrentMarble(move);
    int value = *remove_point;

    // Remove the marble
    Data.erase(remove_point);
    
    // Adjust the current marble pointer if this would shift it along one.  Leave iterator unchanged.
    if (index <= CurrentMarbleIndex) --CurrentMarbleIndex;

    return value;
}

// Determines the index of the element "offset" elements from "Index".  Offset
// can be positive (for CW traversal) or negative (for CCW traversal)
int Board::IndexOffset(int index, int offset) const
{
    const int n = static_cast<int>(Data.size());
    int ix = ((index + offset) % n);
    if (ix < 0) ix += n;

    return ix;
}

// Set the current marble to the given board index.  Traverses from begin() so not efficient in large collections
void Board::SetCurrentMarble(int index)
{
    CurrentMarbleIndex = index;
    CurrentMarble = std::next(Data.begin(), index);
}

// Move the current marble forwards to the given location
void Board::MoveCurrentMarbleForwardToPosition(TData::iterator it)
{
    auto dist = std::distance(CurrentMarble, it);

    CurrentMarbleIndex += static_cast<int>(dist);
    CurrentMarble = it;
}

// Move the current marble backwards to the given location
void Board::MoveCurrentMarbleBackwardToPosition(TData::iterator it)
{
    auto dist = std::distance(it, CurrentMarble);

    CurrentMarbleIndex -= static_cast<int>(dist);
    CurrentMarble = it;
}

// Move the current marble position by the given offset, positive (CW) or negative (CCW)
void Board::MoveCurrentMarble(int offset)
{
    int move = IndexOffset(CurrentMarbleIndex, offset) - CurrentMarbleIndex;

    CurrentMarbleIndex += move;
    CurrentMarble = GetOffsetFromCurrentMarble(move);
}

// Returns an iterator to the position 'offset' from the current marble
Board::TData::iterator Board::GetOffsetFromCurrentMarble(int offset)
{
    if (offset < 0)
    {
        return std::prev(CurrentMarble, -offset);
    }
    else
    {
        return std::next(CurrentMarble, offset);
    }
}

// Debug output
std::string Board::str() const
{
    std::stringstream ss;

    auto it = Data.begin();
    size_t n = std::min(Data.size(), static_cast<size_t>(1000U));

    for (size_t i = 0; i < n; ++i, ++it)
    {
        if (i == CurrentMarbleIndex) ss << "(" << *it << ") ";
        else ss << *it << " ";
    }

    if (Data.size() > n) ss << "{...}";

    return ss.str();
}

