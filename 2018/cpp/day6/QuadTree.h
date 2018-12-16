#pragma once

#include <vector>
#include <array>
#include <memory>
#include "Point.h"
#include "Grid.h"

class QuadTree
{
public:
    const QuadTree & Parent;                                // Parent node, or null for root
    std::array<std::unique_ptr<QuadTree>, 4U> Children;     // Child nodes, either all null or all non-null
    std::vector<Point> Points;                              // Vector of interest points in this node (leaf nodes only)

    QuadTree(const QuadTree & parent, Point centre, Point size);

    static const int MAX_OCCUPANCY = 4;             // Maxmium number of interest points within a node before it subdivides
    static const Point MIN_NODE_SIZE;               // Stop subdividing if nodes reach this minimum size; overrides max occupancy

    static const int CHILD_NXNY = 0;
    static const int CHILD_NXPY = 1;
    static const int CHILD_PXNY = 2;
    static const int CHILD_PXPY = 3;

    inline bool IsLeaf(void) const { return (Children[0] == nullptr); }
    inline bool IsBranch(void) const { return !IsLeaf(); }

    int ChildIndexForPoint(const Point & point) const;

    void Add(const Point & point);

private:

    Point m_centre;
    Point m_size;

    void Subdivide(void);

};
