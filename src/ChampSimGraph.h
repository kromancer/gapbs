#ifndef CHAMPSIMGRAPH_H
#define CHAMPSIMGRAPH_H

#include "ChampSimIterator.h"
#include "benchmark.h"

class ChampSimGraph : public Graph {
public:
  ChampSimGraph(Graph &&g) : Graph(std::move(g)) {}

  ChampSimIterator
  out_neigh_with_cache_monitoring(NodeID n, size_t start_offset = 0) const {
    return ChampSimIterator(n, GetOffsets(), start_offset);
  }
};

#endif // CHAMPSIMGRAPH_H
