#include "Tree.h"
#include <sstream>
#include <numeric>
#include <algorithm>


Tree::Tree(Tree *parent) 
    : 
    Parent(parent), ChildCount(0U), MetadataCount(0U), Value(0U), Evaluated(false)
{ 
}

bool Tree::HasAllChildren(void) const 
{ 
    return (Children.size() == ChildCount); 
}

bool Tree::HasAllMetadata(void) const 
{ 
    return (Metadata.size() == MetadataCount); 
}

// Calculates the node value based only on its metadata and direct children; i.e. no tree traversal is performed
void Tree::CalculateValue(void)
{
    if (IsLeaf())
    {
        Value = std::accumulate(Metadata.begin(), Metadata.end(), 0, [](int acc, int el) { return (acc + el); });
    }
    else
    {
        Value = std::accumulate(Metadata.begin(), Metadata.end(), 0, 
            [this](int acc, int el) { return (acc + (el <= Children.size() ? Children[el-1]->Value : 0)); });
    }

    Evaluated = true;
}

// Reset the tree and all its children
void Tree::Reset(void)
{
    std::vector<Tree *> nodes;
    nodes.push_back(this);

    // Vector-based emulation of recursive traversal
    while (!nodes.empty())
    {
        Tree * node = nodes.back();
        nodes.pop_back();

        // Push all this node's children onto the stack in its place
        std::for_each(node->Children.begin(), node->Children.end(), [&nodes](std::unique_ptr<Tree> & el) { nodes.push_back(el.get()); });

        // Reset the node
        Value = 0;
        Evaluated = false;
    }
}