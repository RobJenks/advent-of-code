#pragma once

#include <assert.h>
#include "../common/Vec2.h"

enum class RoomDir : int
{
    North = 0,
    East,
    South,
    West,

    None,
};

class RoomConn
{
public:
    static const int None = 0;
    static const int North = (1 << static_cast<int>(RoomDir::North));
    static const int East = (1 << static_cast<int>(RoomDir::East));
    static const int South = (1 << static_cast<int>(RoomDir::South));
    static const int West = (1 << static_cast<int>(RoomDir::West));
};

inline RoomDir GetRoomDir(char ch)
{
    switch (ch)
    {
        case 'N':       return RoomDir::North;
        case 'E':       return RoomDir::East;
        case 'S':       return RoomDir::South;
        case 'W':       return RoomDir::West;

        default:
            assert(false);
            return RoomDir::None;
        }
}

inline char RoomDirSchematic(RoomDir dir)
{
    switch (dir)
    {
        case RoomDir::North:    return 'N';
        case RoomDir::East:     return 'E';
        case RoomDir::South:    return 'S';
        case RoomDir::West:     return 'W';

        default:
            assert(false);
            return '?';
    }
}

inline RoomDir OppositeRoomDir(RoomDir dir)
{
    switch (dir)
    {
        case RoomDir::North:    return RoomDir::South;
        case RoomDir::East:     return RoomDir::West;
        case RoomDir::South:    return RoomDir::North;
        case RoomDir::West:     return RoomDir::East;

        default:
            assert(false);
            return RoomDir::None;
        }
}

inline Vec2<int> RoomDirOffset(RoomDir dir)
{
    switch (dir)
    {
        case RoomDir::North:    return { 0, -1 };
        case RoomDir::East:     return { +1, 0 };
        case RoomDir::South:    return { 0, +1 };
        case RoomDir::West:     return { -1, 0 };

        default:
            assert(false);
            return { 0,0 };
    }
}
