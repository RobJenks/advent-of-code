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


    Tree(Tree *parent);

    bool                                        HasAllChildren(void) const;
    bool                                        HasAllMetadata(void) const;

};
