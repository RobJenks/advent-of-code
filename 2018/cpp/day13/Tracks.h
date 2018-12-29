#pragma once

#include <vector>
#include <optional>
#include "../common/Vec2.h"
#include "Cell.h"
#include "Car.h"


class Tracks
{
public:

    Tracks(Vec2<int> size);

    typedef std::vector<Cell>   TData;
    TData                       Data;

    inline size_t       Index(int x, int y) const { return (x + (y * m_size.x)); }
    inline Vec2<int>    Coord(size_t index) const { return Vec2(static_cast<int>(index % m_size.x), static_cast<int>(index / m_size.x)); }

    inline Vec2<int>    GetSize(void) const { return m_size; }
    inline size_t       GetCellCount(void) const { return m_count; }

    inline size_t       GetCycleCount(void) const { return m_cycles; }

    void                PerformPostProcessing(void);
    void                RegisterCar(size_t index, char schematic);

    void                Simulate(void);

    static const size_t NO_CELL = static_cast<size_t>(0U) - static_cast<size_t>(1U);
    size_t              GetLeft(size_t index) const;
    size_t              GetRight(size_t index) const;
    size_t              GetUp(size_t index) const;
    size_t              GetDown(size_t index) const;

    size_t              GetNeighbour(size_t index, Cell::CellState direction) const;

    void                TurnCarOnCellEntry(Car & car, Cell & dest, Cell::CellState entry_direction);

    bool                CrashesHaveOccurred(void) const { return !m_crashes.empty(); }
    const std::vector<size_t> & GetCrashLocations(void) const { return m_crashes; }

    static Cell::CellState  OppositeDirection(Cell::CellState direction);

    static Cell::CellState  TrackDirection(Car::CarDirection direction);
    static Car::CarDirection CarDirection(Cell::CellState direction);

    static Cell::CellState  TrackFromCarDirection(Car::CarDirection direction);

    void                VerifyState(void) const;

    std::string         str(void) const;


private:

    Cell::CellState     ResolveCornerBS(size_t index) const;
    Cell::CellState     ResolveCornerFS(size_t index) const;

    Cell::CellState     GetSurroundingConnections(size_t index) const;

    void                RecordCrash(size_t index, Car::IndexType entering_car);

private:


    const Vec2<int>         m_size;
    const size_t            m_count;

    std::vector<Car>        m_cars;
    std::vector<size_t>     m_crashes;
    size_t                  m_cycles;
    

};