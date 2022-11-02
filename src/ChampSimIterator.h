#ifndef CHAMPSIMITERATOR_H
#define CHAMPSIMITERATOR_H

#include <cassert>

#include "champsim_adapter.h"
#include "types.h"

/** @brief Stimulates the memory hierarchy when accessing the offset and
 * neighbor arrays
 */
class ChampSimIterator {
  typedef NodeID value_type;

private:
  NodeID n_;
  NodeID const *const *offsets_;
  NodeID const *index_;

public:
  ChampSimIterator(NodeID n, const NodeID *const *offsets,
                   size_t start_from = 0)
      : n_(n), offsets_(offsets), index_(offsets[n]) {

    ptrdiff_t max_span_ = offsets[n_ + 1] - offsets[n_];
    assert(max_span_ >= 0);
    size_t max_span = static_cast<size_t>(max_span_);

    index_ += start_from > max_span ? max_span : start_from;
  }

  long operator-(const ChampSimIterator &other) {
    assert(other.offsets_ == offsets_);
    return other.index_ - index_;
  }

  friend bool operator!=(const ChampSimIterator &a, const ChampSimIterator &b) {
    assert(a.offsets_ == b.offsets_);
    return a.index_ != b.index_;
  }

  friend bool operator==(const ChampSimIterator &a, const ChampSimIterator &b) {
    assert(a.offsets_ == b.offsets_);
    return a.index_ == b.index_;
  }

  NodeID const *operator->() { return index_; }

  ChampSimIterator &operator+(size_t incr) {
    index_ += incr;
    return *this;
  }

  ChampSimIterator &operator++() {
    index_++;
    return *this;
  }

  /** @brief The neighborhood of a vertex n lies between [ `offsets_[n]`,
     `offsets_[n + 1]` ) Stimulate the mem hierarchy with a cpu core LOAD
     operations for the \b lower bound.
  */
  ChampSimIterator begin() {
    load(reinterpret_cast<uint64_t>(offsets_[n_]));
    return ChampSimIterator(n_, offsets_);
  }

  /** @brief The neighborhood of a vertex n lies between [ `offsets_[n]`,
     `offsets_[n + 1]` ) Stimulate the mem hierarchy with a cpu core LOAD
     operations for the \b upper bound.
  */
  ChampSimIterator end() {
    load(reinterpret_cast<uint64_t>(offsets_[n_ + 1]));
    return ChampSimIterator(
        n_, offsets_, static_cast<size_t>(offsets_[n_ + 1] - offsets_[n_]));
  }

  /** @brief Overload of the dereference operator for a neighborhood iterator.
             Stimulate the mem hierarchy with a cpu core LOAD operation.
   */
  NodeID operator*() {
    load(reinterpret_cast<uint64_t>(index_));
    return *index_;
  }
};

#endif // CHAMPSIMITERATOR_H
