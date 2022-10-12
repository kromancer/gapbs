#include <atomic>
#include <iostream>
#include <queue>
#include <thread>
#include <vector>

using namespace std;

class Graph {
public:
  Graph();
  void bfs();

private:
  vector<size_t> *offsetList;
  vector<size_t> *edgeList;
  queue<size_t> *workQueue;
  vector<atomic_flag> *isVisited;
  void visitNeighbors(size_t v);
};

Graph::Graph() {
  offsetList = new vector<size_t> {0, 2, 5, 8, 10, 11, 13, 14};
  edgeList = new vector<size_t> {2, 4, 0, 3, 6, 1, 4, 6, 2, 4, 5, 2, 6, 1};
  isVisited = new vector<atomic_flag>(offsetList->size() - 1);
}

void Graph::bfs() {
  workQueue = new queue<size_t>;
  workQueue->push(0);

  cout << "Visited " << 0 << endl;
  isVisited->at(0).test_and_set();

  while (!workQueue->empty()) {
    vector<thread> pool;
    while (!workQueue->empty()) {
      size_t v = workQueue->front();
      workQueue->pop();
      pool.emplace_back(&Graph::visitNeighbors, this, v);
    }

    for (auto &t: pool) {
      t.join();
    }
  }
}

void Graph::visitNeighbors(size_t v) {
  for (auto i = offsetList->at(v); i < offsetList->at(v + 1); i++) {
    size_t neigh = edgeList->at(i);
    bool isUpdated = isVisited->at(neigh).test_and_set();
    if (!isUpdated) {
      cout << "Visited " << neigh << endl;
      workQueue->push(neigh);
    }
  }
}

int main() {

  Graph MyGraph = Graph();
  MyGraph.bfs();

  return 0;
}
