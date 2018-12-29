#pragma once

#include <cstdint>
#include <string>
#include "Car.h"

class Cell
{
public:

    typedef uint8_t CellState;
    struct States
    {
        static const CellState None = 0;
        static const CellState Left = 1 << 0;
        static const CellState Up = 1 << 1;
        static const CellState Right = 1 << 2;
        static const CellState Down = 1 << 3;

        // States which must be determined in a post-processing step since they depend on surrounding cells
        static const CellState UnconfirmedCornerBS = 1 << 4;
        static const CellState UnconfirmedCornerFS = 1 << 5;
        static const CellState UnconfirmedCarCell = 1 << 6;

        // Collision between two cars
        static const CellState Crash = 1 << 7;
    };

    inline Cell(void) : m_state(States::None), m_car(Car::NONE) { }
    
    inline CellState ConnectionState(void) const { return m_state; }
    inline void AddConnections(CellState directions) { m_state |= directions; }
    inline void SetConnections(CellState directions) { m_state = directions; }
    inline bool ConnectsTo(CellState direction) const { return ((m_state & direction) == direction); }
    inline bool IsIntersection(void) const { return ConnectsTo(States::Left | States::Up | States::Right | States::Down); }

    static CellState GetConnectionsFromSchematic(char cell);
    static std::string CellStateStr(CellState state);

    inline void SetCar(Car::IndexType car) { m_car = car; }
    inline bool HasCar(void) const { return (m_car != Car::NONE); }
    inline Car::IndexType GetCar(void) const { return m_car; }
    inline void RemoveCar(void) { m_car = Car::NONE; }

    char GetStateSchematic(void) const;

private:
    
    CellState           m_state;
    Car::IndexType      m_car;
};