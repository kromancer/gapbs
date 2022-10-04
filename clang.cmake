set(CMAKE_C_COMPILER clang)
set(CMAKE_CXX_COMPILER clang++)
set(CMAKE_CXX_FLAGS "-Weverything -Wno-c++98-compat -Wno-sign-compare -Wno-atomic-implicit-seq-cst -Wno-shorten-64-to-32 -Wno-undefined-reinterpret-cast -Wno-unused-parameter -Wno-shadow-field-in-constructor -Wno-missing-noreturn -Wno-missing-prototypes -Wno-sign-conversion -Wno-implicit-int-conversion -Wno-padded -Wno-non-virtual-dtor -Wno-weak-vtables -Wno-old-style-cast -fopenmp")
