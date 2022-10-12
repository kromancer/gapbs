#ifndef CHAMPSIM_ADAPTER_H
#define CHAMPSIM_ADAPTER_H

#include <cstdint>

void initializeL1DCache();
void deinitL1DCache();
void load(uint64_t addr);
void printCacheStats();

#endif // CHAMPSIM_ADAPTER_H
