#pragma once

#include <list>


class Board
{
public:

    typedef std::list<int> TData;

    TData Data;
    int CurrentMarble;

    Board(void);

    void NewGame(void);

    // Place a marble at the given offset from the current marble.  Returns the index of the marble that was placed
    int PlaceMarble(int marble, int offset);

    // Remove a marble at the given offset from the current marble.  Returns the value of 
    // the marble that was removed
    int RemoveMarble(int offset);

    // Set the current marble to the given board index
    void SetCurrentMarble(int index);

    // Move the current marble position by the given offset, positive (CW) or negative (CCW)
    void MoveCurrentMarble(int offset);

    // Determines the index of the element "offset" elements from "Index".  Offset
    // can be positive (for CW traversal) or negative (for CCW traversal)
    int IndexOffset(int index, int offset) const;

    // Debug output
    std::string str() const;
};