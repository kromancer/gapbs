// Copyright (c) 2015, The Regents of the University of California (Regents)
// See LICENSE.txt for license details

#ifndef READER_H_
#define READER_H_

#include <iostream>
#include <fstream>
#include <string>
#include <sstream>
#include <type_traits>

#include "util.h"
#include "pvector.h"


/*
GAP Benchmark Suite
Class:  Reader
Author: Scott Beamer

Given filename, returns an edgelist or the entire graph (if serialized)
 - Intended to be called from Builder
 - Determines file format from the filename's suffix
 - If the input graph is serialized (.sg or .wsg), reads the graph
   directly into the returned graph instance
 - Otherwise, reads the file and returns an edgelist
*/


template <typename NodeID_, typename DestID_ = NodeID_,
          typename WeightT_ = NodeID_, bool invert = true>
class Reader {
  typedef EdgePair<NodeID_, DestID_> Edge;
  typedef pvector<Edge> EdgeList;
  std::string filename_;

 public:
  explicit Reader(std::string filename) : filename_(filename) {}

  std::string GetSuffix() {
    std::size_t suff_pos = filename_.rfind('.');
    if (suff_pos == std::string::npos) {
      std::cout << "Could't find suffix of " << filename_ << std::endl;
      std::exit(-1);
    }
    return filename_.substr(suff_pos);
  }

  EdgeList ReadInEL(std::ifstream &in) {
    EdgeList el;
    NodeID_ u, v;
    while (in >> u >> v) {
      el.push_back(Edge(u, v));
    }
    return el;
  }

  EdgeList ReadInWEL(std::ifstream &in) {
    EdgeList el;
    NodeID_ u;
    NodeWeight<NodeID_, WeightT_> v;
    while (in >> u >> v) {
      el.push_back(Edge(u, v));
    }
    return el;
  }

  EdgeList ReadInGR(std::ifstream &in) {
    EdgeList el;
    char c;
    NodeID_ u;
    NodeWeight<NodeID_, WeightT_> v;
    while (!in.eof()) {
      c = in.peek();
      if (c == 'a') {
        in >> c >> u >> v;
        el.push_back(Edge(u, v));
      } else {
        in.ignore(200, '\n');
      }
    }
    return el;
  }

  // converts vertex numbering from 1..N to 0..N-1
  EdgeList ReadInMetis(std::ifstream &in, bool &needs_weights) {
    EdgeList el;
    NodeID_ num_nodes, num_edges;
    char c;
    std::string line;
    bool read_weights = false;
    while (true) {
      c = in.peek();
      if (c == '%') {
        in.ignore(200, '\n');
      } else {
        std::getline(in, line, '\n');
        std::istringstream header_stream(line);
        header_stream >> num_nodes >> num_edges;
        header_stream >> std::ws;
        if (!header_stream.eof()) {
          int32_t fmt;
          header_stream >> fmt;
          if (fmt == 1) {
            read_weights = true;
          } else if ((fmt != 0) && (fmt != 100)) {
            std::cout << "Do not support METIS fmt type: " << fmt << std::endl;
            std::exit(-20);
          }
        }
        break;
      }
    }
    NodeID_ u = 0;
    while (u < num_nodes) {
      c = in.peek();
      if (c == '%') {
        in.ignore(200, '\n');
      } else {
        std::getline(in, line);
        if (line != "") {
          std::istringstream edge_stream(line);
          if (read_weights) {
            NodeWeight<NodeID_, WeightT_> v;
            while (!edge_stream.eof()) {
              edge_stream >> v;
              edge_stream >> std::ws;
              v.v -= 1;
              el.push_back(Edge(u, v));
            }
          } else {
            NodeID_ v;
            while (!edge_stream.eof()) {
              edge_stream >> v;
              edge_stream >> std::ws;
              el.push_back(Edge(u, v - 1));
            }
          }
        }
        u++;
      }
    }
    needs_weights = !read_weights;
    return el;
  }

  EdgeList ReadFile(bool &needs_weights) {
    Timer t;
    t.Start();
    EdgeList el;
    std::string suffix = GetSuffix();
    std::ifstream file(filename_);
    if (!file.is_open()) {
      std::cout << "Couldn't open file " << filename_ << std::endl;
      std::exit(-2);
    }
    if (suffix == ".el") {
      el = ReadInEL(file);
    } else if (suffix == ".wel") {
      needs_weights = false;
      el = ReadInWEL(file);
    } else if (suffix == ".gr") {
      needs_weights = false;
      el = ReadInGR(file);
    } else if (suffix == ".graph") {
      el = ReadInMetis(file, needs_weights);
    } else {
      std::cout << "Unrecognized suffix: " << suffix << std::endl;
      std::exit(-3);
    }
    file.close();
    t.Stop();
    PrintTime("Read Time", t.Seconds());
    return el;
  }

  CSRGraph<NodeID_, DestID_, invert> ReadSerializedGraph() {
    bool weighted = GetSuffix() == ".wsg";
    if (!std::is_same<NodeID_, SGID>::value) {
      std::cout << "serialized graphs only allowed for 32bit" << std::endl;
      std::exit(-5);
    }
    if (!weighted && !std::is_same<NodeID_, DestID_>::value) {
      std::cout << ".sg not allowed for weighted graphs" << std::endl;
      std::exit(-5);
    }
    if (weighted && std::is_same<NodeID_, DestID_>::value) {
      std::cout << ".wsg only allowed for weighted graphs" << std::endl;
      std::exit(-5);
    }
    if (weighted && !std::is_same<WeightT_, SGID>::value) {
      std::cout << ".wsg only allowed for int32_t weights" << std::endl;
      std::exit(-5);
    }
    std::ifstream file(filename_);
    if (!file.is_open()) {
      std::cout << "Couldn't open file " << filename_ << std::endl;
      std::exit(-6);
    }
    Timer t;
    t.Start();
    bool directed;
    SGOffset num_nodes, num_edges;
    DestID_ **index = nullptr, **inv_index = nullptr;
    DestID_ *neighs = nullptr, *inv_neighs = nullptr;
    file.read(reinterpret_cast<char*>(&directed), sizeof(bool));
    file.read(reinterpret_cast<char*>(&num_edges), sizeof(SGOffset));
    file.read(reinterpret_cast<char*>(&num_nodes), sizeof(SGOffset));
    pvector<SGOffset> offsets(num_nodes+1);
    neighs = new DestID_[num_edges];
    std::streamsize num_index_bytes = (num_nodes+1) * sizeof(SGOffset);
    std::streamsize num_neigh_bytes = num_edges * sizeof(DestID_);
    file.read(reinterpret_cast<char*>(offsets.data()), num_index_bytes);
    file.read(reinterpret_cast<char*>(neighs), num_neigh_bytes);
    index = CSRGraph<NodeID_, DestID_>::GenIndex(offsets, neighs);
    if (directed && invert) {
      inv_neighs = new DestID_[num_edges];
      file.read(reinterpret_cast<char*>(offsets.data()), num_index_bytes);
      file.read(reinterpret_cast<char*>(inv_neighs), num_neigh_bytes);
      inv_index = CSRGraph<NodeID_, DestID_>::GenIndex(offsets, inv_neighs);
    }
    file.close();
    t.Stop();
    PrintTime("Read Time", t.Seconds());
    if (directed)
      return CSRGraph<NodeID_, DestID_, invert>(num_nodes, index, neighs,
                                                inv_index, inv_neighs);
    else
      return CSRGraph<NodeID_, DestID_, invert>(num_nodes, index, neighs);
  }
};

#endif  // READER_H_