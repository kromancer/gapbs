cmake_minimum_required(VERSION 3.22)

project(gapbs LANGUAGES CXX)

add_executable(pr src/pr.cc)
target_compile_options(pr PRIVATE "-std=c++17" "-ggdb3" "-O0")

add_executable(bfs src/bfs.cc)
target_compile_options(bfs PRIVATE "-std=c++17" "-ggdb3" "-O0")

add_executable(naive_bfs src/naive_bfs.cc)
target_compile_options(naive_bfs PRIVATE "-std=c++17" "-ggdb3" "-O0")
