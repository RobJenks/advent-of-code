#include "day8.h"
#include <iostream>
#include <vector>
#include <sstream>
#include <numeric>


void Day8::Run(void) const
{
    std::cout << "\nDay 8:\n";

    Part1();
}

void Day8::Part1(void) const
{
    std::string input = ReadInput(fs::path("day8/input.txt"));

    auto tree = BuildTree(input);
    auto result = AggregateMetadata(tree.get());

    std::cout << "Part 1 result = " << result << "\n";
}

std::unique_ptr<Tree> Day8::BuildTree(const std::string & input) const
{
    int value;
    std::unique_ptr<Tree> root = std::make_unique<Tree>(nullptr);
    
    std::istringstream ss(input);
    ss >> root->ChildCount >> root->MetadataCount;

    auto node = std::ref(*root);
    
    // Read values into the current node in order, traversing into children and back up where required
    while (ss)
    {
        if (!node.get().HasAllChildren())
        {
            // If the current node is missing children, populate this preferentially.  Traverse into the child and continue
            std::unique_ptr<Tree> child = std::make_unique<Tree>(&node.get());
            ss >> child->ChildCount;
            ss >> child->MetadataCount;

            node.get().Children.emplace_back(std::move(child));
            node = std::ref(*node.get().Children.back());
        }
        else if (!node.get().HasAllMetadata())
        {
            // If the current node has all children, the remaining values must be node metadata
            ss >> value;
            node.get().Metadata.push_back(value);
        }
        else
        {
            // If we have a complete set of children and metadata, we have reached the end of this node definition
            // and can traverse back to our parent and continue
            if (node.get().Parent == nullptr) break;
            node = std::ref(*node.get().Parent);
        }
    }

    return root;
}

int Day8::AggregateMetadata(const Tree *tree) const 
{
    int total = 0;

    std::vector<const Tree *> nodes;
    nodes.push_back(tree);

    // Vector-based emulation of recursive traversal
    while (!nodes.empty())
    {
        const Tree * node = nodes.back();
        nodes.pop_back();

        // Push all this node's children onto the stack in its place
        std::for_each(node->Children.begin(), node->Children.end(), [&nodes](const auto & el) { nodes.push_back(el.get()); });

        // Accumulate all metadata from this node
        total += std::accumulate(node->Metadata.begin(), node->Metadata.end(), 0, [](int acc, int el) { return (acc + el); });
    }

    return total;
}