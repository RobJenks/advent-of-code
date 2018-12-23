#pragma once

#include <list>


class Board
{
public:

    typedef std::list<int> TData;

    TData Data;
    TData::iterator CurrentMarble;
    int CurrentMarbleIndex;

    Board(void);

    void NewGame(void);

    // Place a marble at the given offset from the current marble.  Returns an iterator to the marble that was placed
    std::tuple<Board::TData::iterator, bool> PlaceMarble(int marble, int offset);

    // Remove a marble at the given offset from the current marble.  Returns the value of 
    // the marble that was removed
    int RemoveMarble(int offset);

    // Set the current marble to the given board index
    void SetCurrentMarble(int index);

    // Move the current marble to the given iterator
    void MoveCurrentMarbleForwardToPosition(TData::iterator it);
    void MoveCurrentMarbleBackwardToPosition(TData::iterator it);
    inline void MoveCurrentMarbleToPosition(TData::iterator it, bool forward) 
    { 
        (forward ? MoveCurrentMarbleForwardToPosition(it) : MoveCurrentMarbleBackwardToPosition(it)); 
    }

    // Returns an iterator to the position 'offset' from the current marble
    TData::iterator GetOffsetFromCurrentMarble(int offset);

    // Move the current marble position by the given offset, positive (CW) or negative (CCW)
    void MoveCurrentMarble(int offset);

    // Determines the index of the element "offset" elements from "Index".  Offset
    // can be positive (for CW traversal) or negative (for CCW traversal)
    int IndexOffset(int index, int offset) const;

    // Debug output
    std::string str() const;
};