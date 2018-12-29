#pragma once

#include <cstdint>

class Car
{
public:

    typedef uint8_t CarDirection;
    struct Directions
    {
        static const CarDirection Unknown = 0;
        static const CarDirection Left = 1 << 0;
        static const CarDirection Up = 1 << 1;
        static const CarDirection Right = 1 << 2;
        static const CarDirection Down = 1 << 3;
    };


    // No test or problem uses >= 255 cars, so we can safely use a uint8 & keep one value reserved
    typedef uint8_t IndexType;
    static const IndexType NONE = static_cast<IndexType>(0U) - static_cast<IndexType>(1U);

    Car(void);

    inline size_t GetCell(void) const { return m_cell; }
    inline void SetCell(size_t cell) { m_cell = cell; }

    inline CarDirection GetDirection(void) const { return m_direction; }
    inline void SetDirection(CarDirection direction) { m_direction = direction; }

    void TakeIntersection(void);


    static CarDirection RotateDirectionCCW(CarDirection direction);
    static CarDirection RotateDirectionCW(CarDirection direction);

    static CarDirection DirectionFromSchematic(char schematic);
    static char GetSchematicForDirection(CarDirection direction);

    inline char GetSchematic(void) const { return GetSchematicForDirection(m_direction); }

private:

    size_t          m_cell;
    CarDirection    m_direction;
    CarDirection    m_nextturn;

};