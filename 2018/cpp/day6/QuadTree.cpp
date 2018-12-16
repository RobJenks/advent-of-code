#include "QuadTree.h"

const Point QuadTree::MIN_NODE_SIZE = Point(5, 5);

QuadTree::QuadTree(const QuadTree & parent, Point centre, Point size)
    :
    Parent(parent),
    m_centre(centre), 
    m_size(size), 
    Children{ nullptr, nullptr, nullptr, nullptr }
{
}

int QuadTree::ChildIndexForPoint(const Point & point) const
{
    if (point.x < m_centre.x)
    {
        if (point.y < m_centre.y)
        {
            return CHILD_NXNY;
        }
        else // y >= centre
        {
            return CHILD_NXPY;
        }
    }
    else     // x >= centre
    {
        if (point.y < m_centre.y)
        {
            return CHILD_PXNY;
        }
        else // y >= centre
        {
            return CHILD_PXPY;
        }
    }
}

void QuadTree::Add(const Point & point)
{
    // Subdivide if this is a leaf node at max occupancy, as long as it is not already at minimum permitted size
    if (IsLeaf() && Points.size() == MAX_OCCUPANCY && m_size > MIN_NODE_SIZE)
    {
        Subdivide();
    }

    if (IsBranch())
    {
        // Pass down to the appropriate child if this is not a leaf node
        Children[ChildIndexForPoint(point)]->Add(point);
        return;
    }
    else
    {
        // Otherwise simply add to the item collection for this node
        Points.push_back(point);
    }
}

void QuadTree::Subdivide(void)
{
    Point child_size = (m_size / Point(2, 2));
    Point centre_n = (m_centre - (m_size / Point(2, 2)));
    Point centre_p = (m_centre + (m_size / Point(2, 2)));

    Children[CHILD_NXNY] = std::make_unique<QuadTree>(*this, centre_n, child_size);
    Children[CHILD_NXPY] = std::make_unique<QuadTree>(*this, Point(centre_n.x, centre_p.y), child_size);
    Children[CHILD_PXNY] = std::make_unique<QuadTree>(*this, Point(centre_p.x, centre_n.y), child_size);
    Children[CHILD_PXPY] = std::make_unique<QuadTree>(*this, centre_p, child_size);
}