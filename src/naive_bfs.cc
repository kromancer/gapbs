#include <iostream>
#include <vector>
#include <queue>

#include "benchmark.h"
#include "bitmap.h"
#include "builder.h"
#include "command_line.h"
#include "graph.h"
#include "platform_atomics.h"
#include "pvector.h"
#include "sliding_queue.h"
#include "timer.h"
#include "types.h"

#include "champsim_adapter.h"
#include "ChampSimGraph.h"
#include "PrefetchAddressGenerator.h"

using namespace std;

/** @brief Visits graph in BFS manner
 */
static PrefetchAddressGenerator PrefetchAddressGenerator(const ChampSimGraph &g, NodeID source) {

  // TODO: We are not monitoring frontier accesses
  queue<NodeID> frontier;
  frontier.push(source);

  // TODO: We are not monitoring parent accesses
  pvector<NodeID> parent(g.num_nodes(), INVALID_NODE_ID);
  parent[source] = source;

  NodeID const * const *offsets = g.GetOffsets();

  while (!frontier.empty()) {
    auto u = frontier.front(); frontier.pop();

    const NodeID *start = offsets[u];
    co_yield reinterpret_cast<uint64_t>(&offsets[u]);

    const NodeID *end = offsets[u + 1];
    co_yield reinterpret_cast<uint64_t>(&offsets[u + 1]);

    for (NodeID v = *start; start < end; start++) {
      co_yield reinterpret_cast<uint64_t>(start);
      if (parent[v] == INVALID_NODE_ID) {
        parent[v] = u;
        frontier.push(v);
      }
    }

  }

  co_return;
}

/** @brief The benchmark: Constructs a BFS tree of the given graph from the given source
 */
pvector<NodeID> DOBFS(const ChampSimGraph &g, NodeID source) {

  // Notice the memory system hierarchy takes in
  // a "prefetching" schedule
  auto gen = PrefetchAddressGenerator(g, source);
  initChampSim(gen);

  // TODO: We are not monitoring frontier accesses
  queue<NodeID> frontier;
  frontier.push(source);

  // TODO: We are not monitoring parent accesses
  pvector<NodeID> parent(g.num_nodes(), INVALID_NODE_ID);
  parent[source] = source;

  while (!frontier.empty()) {
    auto u = frontier.front(); frontier.pop();
    for (auto v : g.out_neigh_with_cache_monitoring(u)) {
      if (parent[v] == INVALID_NODE_ID) {
        parent[v] = u;
        frontier.push(v);
      }
    }
  }

  deinitChampSim();
  printCacheStats();

  return parent;
}

void PrintBFSStats(const Graph &g, const pvector<NodeID> &bfs_tree) {
  int64_t tree_size = 0;
  int64_t n_edges = 0;
  for (NodeID n : g.vertices()) {
    if (bfs_tree[n] != INVALID_NODE_ID) {
      n_edges += g.out_degree(n);
      tree_size++;
    }
  }

  cout << "BFS Tree has " << tree_size << " nodes and ";
  cout << n_edges << " edges" << endl;
}

bool BFSVerifier(const Graph &g, NodeID source, const pvector<NodeID> &parent) {

  (void)g;

  // Get "route" to KTH
  cout << "KTH";
  NodeID next = 4325269;

  while(next != source) {
    next = parent[next];
    cout << "-> " << next;
  }

  cout << endl;

  return true;
}

int main(int argc, char *argv[]) {
  CLApp cli(argc, argv, "breadth-first search");
  if (!cli.ParseArgs())
    return -1;

  Builder b(cli);
  ChampSimGraph g = b.MakeGraph();

  SourcePicker<Graph> sp(g, cli.start_vertex());
  auto BFSBound = [&sp](const ChampSimGraph &g_) { return DOBFS(g_, sp.PickNext()); };

  SourcePicker<Graph> vsp(g, cli.start_vertex());
  auto VerifierBound = [&vsp](const Graph &g_, const pvector<NodeID> &parent) {
    return BFSVerifier(g_, vsp.PickNext(), parent);
  };

  BenchmarkKernel(cli, g, BFSBound, PrintBFSStats, VerifierBound);

  return 0;
}
