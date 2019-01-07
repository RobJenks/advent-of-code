#pragma once

#include <array>
#include "../common/Vec2.h"
#include "RoomDir.h"

class Room
{
public:
    
    int ID;
    Vec2<int> Location;
    std::array<int, 4U> Links;

public:

    static const int NO_ROOM = -1;
    
    // Construct a new room reached from the given previous room 
    inline Room(int id, Room & prev, RoomDir traversal_dir)
        :
        ID(id),
        Links{ NO_ROOM, NO_ROOM, NO_ROOM, NO_ROOM }
    {
        // Bidrectional link
        Links[static_cast<int>(OppositeRoomDir(traversal_dir))] = prev.ID;
        prev.Links[static_cast<int>(traversal_dir)] = id;

        // Determine location based on our source room
        Location = prev.Location + RoomDirOffset(traversal_dir);    
    }

    // Construct a root room with no connectivity
    inline Room(int id, Vec2<int> location)
        :
        ID(id),
        Location(location), 
        Links{ NO_ROOM, NO_ROOM, NO_ROOM, NO_ROOM }
    {
    }


};