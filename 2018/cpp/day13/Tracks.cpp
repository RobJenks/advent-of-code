#include "Tracks.h"
#include <iostream>
#include <cassert>
#include <sstream>
#include <algorithm>

Tracks::Tracks(Vec2<int> size)
    :
    m_size(size),
    m_count(size.x * size.y)
{
    Data.insert(Data.begin(), m_count, Cell());
}

void Tracks::PerformPostProcessing(void)
{
    for (size_t i = 0U; i < m_count; ++i)
    {
        auto state = Data[i].ConnectionState();
        switch (state)
        {
            case Cell::States::UnconfirmedCornerBS:
                Data[i].SetConnections(ResolveCornerBS(i));     
                break;

            case Cell::States::UnconfirmedCornerFS:
                Data[i].SetConnections(ResolveCornerFS(i));
                break;
        }
    }
}

void Tracks::Simulate(void)
{
    // Evaluate cars in cell index order
    std::vector<std::pair<size_t, size_t>> ordered_cars;    // { car_id, cell_index }
    for (size_t i = 0; i < m_cars.size(); ++i) ordered_cars.push_back({ i, m_cars[i].GetCell() });
    std::sort(ordered_cars.begin(), ordered_cars.end(), [](const auto & x0, const auto & x1) { return x0.second < x1.second; });

    for (const auto & c : ordered_cars)
    {
        Car & car = m_cars[c.first];
        
        auto index = car.GetCell();
        Cell & cell = Data[index];
        auto car_id = cell.GetCar();

        auto movedir = TrackDirection(car.GetDirection());
        auto dest_index = GetNeighbour(index, movedir);

        assert(dest_index != NO_CELL);
        Cell & dest = Data[dest_index];

        if (dest.HasCar())
        {
            RecordCrash(index, c.first);
            continue;
        }

        cell.RemoveCar();
        dest.SetCar(car_id);
        car.SetCell(dest_index);

        TurnCarOnCellEntry(car, dest, OppositeDirection(movedir));    // Turn car at end of each cell movement
    }
}

void Tracks::TurnCarOnCellEntry(Car & car, Cell & dest, Cell::CellState entry_direction)
{
    if (dest.IsIntersection())
    {
        car.TakeIntersection();
    }
    else
    {
        // Remove the entry direction from the cell connectivity, and there should be exactly one direction 
        // remaining which the car will take
        auto direction = (dest.ConnectionState() & ~entry_direction);
        car.SetDirection(CarDirection(direction));
    }
}

void Tracks::RecordCrash(size_t index, Car::IndexType entering_car)
{
    Cell & cell = Data[index];
    auto current_car = cell.GetCar();

    cell.RemoveCar();
    Data[m_cars[entering_car].GetCell()].RemoveCar();

    cell.AddConnections(Cell::States::Crash);
    m_crashes.push_back(index);
}

size_t Tracks::GetLeft(size_t index) const
{
    return (index % m_size.x != 0 ? (index - 1) : NO_CELL);
}

size_t Tracks::GetRight(size_t index) const
{
    auto right = (index + 1);
    return (right % m_size.x != 0 ? right : NO_CELL);
}

size_t Tracks::GetUp(size_t index) const
{
    return (index >= m_size.x ? (index - m_size.x) : NO_CELL);
}

size_t Tracks::GetDown(size_t index) const
{
    auto down = (index + m_size.x);
    return (down < m_count ? down : NO_CELL);
}

size_t Tracks::GetNeighbour(size_t index, Cell::CellState direction) const
{
    switch (direction)
    {
        case Cell::States::Left:        return GetLeft(index);
        case Cell::States::Up:          return GetUp(index);
        case Cell::States::Right:       return GetRight(index);
        case Cell::States::Down:        return GetDown(index);
        default:
            assert(false);
            return Cell::States::None;
    }
}

Cell::CellState Tracks::OppositeDirection(Cell::CellState direction)
{
    switch (direction)
    {
        case Cell::States::Left:        return Cell::States::Right;
        case Cell::States::Up:          return Cell::States::Down;
        case Cell::States::Right:       return Cell::States::Left;
        case Cell::States::Down:        return Cell::States::Up;
        default:
            assert(false);
            return Cell::States::None;
    }
}

// Return the set of connections to external cells, based on the state of surrounding cells
Cell::CellState Tracks::GetSurroundingConnections(size_t index) const
{
    size_t adj[4] = { GetLeft(index), GetUp(index), GetRight(index), GetDown(index) };

    Cell::CellState state = Cell::States::None;
    if (adj[0] != NO_CELL && Data[adj[0]].ConnectsTo(Cell::States::Right)) state |= Cell::States::Left;
    if (adj[1] != NO_CELL && Data[adj[1]].ConnectsTo(Cell::States::Down)) state |= Cell::States::Up;
    if (adj[2] != NO_CELL && Data[adj[2]].ConnectsTo(Cell::States::Left)) state |= Cell::States::Right;
    if (adj[3] != NO_CELL && Data[adj[3]].ConnectsTo(Cell::States::Up)) state |= Cell::States::Down;

    return state;
}


Cell::CellState Tracks::ResolveCornerBS(size_t index) const
{
    auto state = GetSurroundingConnections(index);

    if (state == (Cell::States::Up | Cell::States::Right) ||
        state == (Cell::States::Left | Cell::States::Down))
    {
        return state;
    }
    
    assert(false);  // Should not be possible; must be one of the two given corners with no other connectivity
    return Cell::States::None;
}

Cell::CellState Tracks::ResolveCornerFS(size_t index) const
{
    auto state = GetSurroundingConnections(index);

    if (state == (Cell::States::Up | Cell::States::Left) ||
        state == (Cell::States::Right | Cell::States::Down))
    {
        return state;
    }

    assert(false);  // Should not be possible; must be one of the two given corners with no other connectivity
    return Cell::States::None;
}

void Tracks::RegisterCar(size_t index, char schematic)
{
    Car car;
    auto car_direction = Car::DirectionFromSchematic(schematic);

    car.SetCell(index);
    car.SetDirection(car_direction);

    auto car_id = static_cast<Car::IndexType>(m_cars.size());
    m_cars.push_back(car);

    Data[index].SetCar(car_id);
    Data[index].SetConnections(TrackFromCarDirection(car_direction));
}

// Ugh
Cell::CellState Tracks::TrackDirection(Car::CarDirection direction)
{
    switch (direction)
    {
        case Car::Directions::Left:     return Cell::States::Left;
        case Car::Directions::Up:       return Cell::States::Up;
        case Car::Directions::Right:    return Cell::States::Right;
        case Car::Directions::Down:     return Cell::States::Down;
        default:
            assert(false);
            return Cell::States::None;
    }
}
Car::CarDirection Tracks::CarDirection(Cell::CellState direction)
{
    switch (direction)
    {
        case Cell::States::Left:        return Car::Directions::Left;
        case Cell::States::Up:          return Car::Directions::Up;
        case Cell::States::Right:       return Car::Directions::Right;
        case Cell::States::Down:        return Car::Directions::Down;
        default:
            assert(false);
            return Car::Directions::Unknown;
    }
}

Cell::CellState Tracks::TrackFromCarDirection(Car::CarDirection direction)
{
    switch (direction)
    {
        case Car::Directions::Left:
        case Car::Directions::Right:
            return (Cell::States::Left | Cell::States::Right);

        case Car::Directions::Up:
        case Car::Directions::Down:
            return (Cell::States::Up | Cell::States::Down);

        default:
            assert(false);
            return Cell::States::None;
    }
}

void Tracks::VerifyState(void) const
{
    bool valid = true;
    for (size_t i = 0U; i < m_count; ++i)
    {
        if (Data[i].ConnectionState() != GetSurroundingConnections(i))
        {
            std::cout << "Invalid track state: cell " << i << " " << Coord(i).str() << " has connection state '"
                << Cell::CellStateStr(Data[i].ConnectionState()) << "' which does not match adjacent cell connectivity '" 
                << Cell::CellStateStr(GetSurroundingConnections(i)) << "'\n";
            valid = false;
        }
    }

    assert(valid);
}

std::string Tracks::str(void) const
{
    std::stringstream ss;
    int x = 0;
    
    for (const auto & cell : Data)
    {
        if (cell.HasCar())
        {
            ss << m_cars[cell.GetCar()].GetSchematic();
        }
        else
        {
            ss << cell.GetStateSchematic();
        }

        if (++x == m_size.x) { ss << '\n'; x = 0; }
    }

    return ss.str();
}
