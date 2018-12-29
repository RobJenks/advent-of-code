#include "Cell.h"
#include <cassert>

Cell::CellState Cell::GetConnectionsFromSchematic(char cell)
{
    switch (cell)
    {
    case ' ':
        return States::None;
    case '|':
        return (States::Up | States::Down);
    case '-':
        return (States::Left | States::Right);
    case '+':
        return (States::Left | States::Up | States::Right | States::Down);
    case '\\':
        return States::UnconfirmedCornerBS; 
    case '/':
        return States::UnconfirmedCornerFS;

    case '<':
    case '>':
    case '^':
    case 'v':
        return States::UnconfirmedCarCell;

    default:
        assert(false);
        return States::None;
    }
}

std::string Cell::CellStateStr(CellState state)
{
    std::string str;
    
    if ((state & States::Left) != 0) str.push_back('<');
    if ((state & States::Up) != 0) str.push_back('^');
    if ((state & States::Down) != 0) str.push_back('v'); 
    if ((state & States::Right) != 0) str.push_back('>');

    return str;
}

char Cell::GetStateSchematic(void) const
{
    if ((m_state & States::Crash) == States::Crash) return 'X';

    switch (m_state)
    {
        case (States::None):                                                return ' ';
        case (States::Left | States::Right):                                return '-';
        case (States::Up | States::Down):                                   return '|';
        case (States::Left | States::Up | States::Down | States::Right):    return '+';

        case (States::Left | States::Down):
        case (States::Up | States::Right):
            return '\\';

        case (States::Left | States::Up):
        case (States::Down | States::Right):
            return '/';

        default:
            assert(false);
            return '.';
    }
}

