#include "Tree.h"
#include <sstream>


Tree::Tree(Tree *parent) 
    : 
    Parent(parent), ChildCount(0U), MetadataCount(0U) 
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
