#pragma once

#include <vector>
#include "../common/Vec4.h"

class Pt
{
public:

    Vec4<int> Position;
    int Constellation;
    std::vector<int> Links;

public:

    static const int NO_CONST = -1;

    Pt(void) : Position(0), Constellation(NO_CONST) { }
    Pt(const Vec4<int> & pos) : Position(pos), Constellation(NO_CONST) { }

};