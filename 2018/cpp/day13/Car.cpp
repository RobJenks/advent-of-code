#include "Car.h"
#include <cassert>

Car::Car(void)
    :
    m_cell(0UL-1UL),
    m_direction(Directions::Unknown),
    m_nextturn(Directions::Left)
{
}

void Car::TakeIntersection(void)
{
    if (m_nextturn == Directions::Left)
    {
        m_direction = RotateDirectionCCW(m_direction);
        m_nextturn = Directions::Unknown;
    }
    else if (m_nextturn == Directions::Unknown) // == straight ahead
    {
        m_nextturn = Directions::Right;
    }
    else if (m_nextturn == Directions::Right)
    {
        m_direction = RotateDirectionCW(m_direction);
        m_nextturn = Directions::Left;
    }
    else
    {
        assert(false);
    }
}


Car::CarDirection Car::RotateDirectionCCW(CarDirection direction)
{
    switch (direction)
    {
        case Directions::Left:      return Directions::Down;
        case Directions::Up:        return Directions::Left;
        case Directions::Right:     return Directions::Up;
        case Directions::Down:      return Directions::Right;
        default:
            assert(false);
            return Directions::Unknown;
    }
}

Car::CarDirection Car::RotateDirectionCW(CarDirection direction)
{
    switch (direction)
    {
    case Directions::Left:      return Directions::Up;
    case Directions::Up:        return Directions::Right;
    case Directions::Right:     return Directions::Down;
    case Directions::Down:      return Directions::Left;
    default:
        assert(false);
        return Directions::Unknown;
    }
}

Car::CarDirection Car::DirectionFromSchematic(char schematic)
{
    switch (schematic)
    {
        case '<':       return Directions::Left;
        case '^':       return Directions::Up;
        case '>':       return Directions::Right;
        case 'v':       return Directions::Down;
        default:        
            assert(false);
            return Directions::Unknown;
    }
}

char Car::GetSchematicForDirection(CarDirection direction)
{
    switch (direction)
    {
        case Directions::Left:      return '<';
        case Directions::Up:        return '^';
        case Directions::Right:     return '>';
        case Directions::Down:      return 'v';
        default:
            assert(false);
            return Directions::Unknown;
    }
}