#pragma once

#include <vector>
#include <memory>


class Tree
{
public:

    Tree *                                      Parent;

    std::vector<std::unique_ptr<Tree>>          Children;
    size_t                                      ChildCount;

    std::vector<int>                            Metadata;
    size_t                                      MetadataCount;

    int                                         Value;
    bool                                        Evaluated;

    Tree(Tree *parent);

    bool                                        HasAllChildren(void) const;
    bool                                        HasAllMetadata(void) const;

    inline bool                                 IsLeaf(void) const { return (Children.size() == 0); }
    inline bool                                 IsBranch(void) const { return !IsLeaf(); }

    inline bool                                 IsEvaluated(void) const { return Evaluated; }

    void                                        CalculateValue(void);

    void                                        Reset(void);
};
