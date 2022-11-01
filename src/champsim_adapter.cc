#include <algorithm>
#include <cassert>
#include <climits>
#include <cstdint>
#include <iomanip>

#include "block.h"
#include "cache.h"
#include "champsim_constants.h"
#include "memory_class.h"

uint8_t MAX_INSTR_DESTINATIONS = 0;
uint8_t warmup_complete[NUM_CPUS];
uint8_t all_warmup_complete = 0;

static CACHE *L1D = nullptr;

class DummyMemoryRequestConsumer: public MemoryRequestConsumer {
public:
  DummyMemoryRequestConsumer(): MemoryRequestConsumer(UINT_MAX) {}

  int add_pq(PACKET *packet) override { return INT_MAX; }

  /** @brief Responds with the requested packet immediately
   */
  int add_rq(PACKET* packet) override {
    L1D->return_data(packet);
    return INT_MAX;
  }

  int add_wq(PACKET* packet) override { return INT_MAX; }

  uint32_t get_occupancy(uint8_t queue_type, uint64_t address) override {
    (void)queue_type;
    (void)address;

    return 0;
  };

  uint32_t get_size(uint8_t queue_type, uint64_t address) override {
    (void)queue_type;
    (void)address;

    return INT_MAX;
  };
};

static DummyMemoryRequestConsumer *dummyMem = nullptr;

void printCacheStats()
{
  assert(L1D != nullptr);
  cout << L1D->NAME;
  cout << " LOAD: " <<  L1D->sim_access[0][0] << "  HIT: " <<  L1D->sim_hit[0][0] << "  MISS: " << L1D->sim_miss[0][0] << endl;
}

void initializeL1DCache() {
  assert(L1D == nullptr);
  assert(dummyMem == nullptr);

  // Inform the cache that the warmup is complete
  std::fill_n(warmup_complete, sizeof(warmup_complete) / sizeof(warmup_complete[0]), 1);

  dummyMem = new DummyMemoryRequestConsumer();
  L1D = new CACHE(
    "L1D", // NAME
    1.0, // freq_scale
    4, // fill_level
    64, // NUM_SET
    12, // NUM_WAY
    64, // WQ_SIZE
    64, // RQ_SIZE
    8, // PQ_SIZE
    16, // MSHR_SIZE
    4, // hit_lat
    1, // fill_lat
    2, // max_read
    2, // max_write
    LOG2_BLOCK_SIZE, // offset_bits
    0, // pref_load
    1, // wq_full_addr
    0, // va_pref
    0, // pref_act_mask
    dummyMem, // ll (MemoryRequestconsumer*)
    CACHE::pref_t::pprefetcherDerebus,
    CACHE::repl_t::rreplacementDlru); // repl

  L1D->impl_replacement_initialize();
  L1D->impl_prefetcher_initialize();
}

void load(uint64_t addr) {
  PACKET pkt;
  pkt.cpu = 0;
  pkt.fill_level = L1D->fill_level;
  pkt.address = addr;
  pkt.type = LOAD;

  L1D->RQ.push_back_ready(pkt);

  // We need `_operate()` and not just `operate()` to advance the cache's perception of time aka cycle counter
  L1D->_operate();
}

void deinitL1DCache() {
  // Flush any pending operations
  L1D->_operate();
}
