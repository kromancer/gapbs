#ifndef CHAMPSIM_ADAPTER_H
#define CHAMPSIM_ADAPTER_H

#include <cstdint>

#include "erebus.h"
#include "PrefetchAddressGenerator.h"

/** @brief Initialize ChampSim which is used for the memory hierarchy simulation.
    @param gen - A cache line address generator.
 */
void initChampSim(PrefetchAddressGenerator &gen);

void deinitChampSim();
void load(uint64_t addr);
void printCacheStats();

#endif // CHAMPSIM_ADAPTER_H
