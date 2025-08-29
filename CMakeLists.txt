cmake_minimum_required(VERSION 4.0)
project(zeus)

add_subdirectory(parser)

set(CMAKE_CXX_STANDARD 20)


find_package(LLVM 18.1.0 REQUIRED CONFIG)

message(STATUS " Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")
message(STATUS "Compiler: ${CMAKE_CXX_COMPILER_ID}")
include_directories(${LLVM_INCLUDE_DIRS} os linker)
add_definitions(${LLVM_DEFINITIONS})
# Find and link LLD libraries
llvm_map_components_to_libnames(llvm_libs support core irreader native nativecodegen passes)


if (${CMAKE_CXX_COMPILER_ID} STREQUAL "MSVC")
    set_property(TARGET ${PROJECT_NAME} PROPERTY
            MSVC_RUNTIME_LIBRARY "MultiThreaded$<$<CONFIG:Debug>:Debug>")
endif ()
if (UNIX)
    # G++
    file(GLOB LINKER_SRC linker/unix/linker.cpp os/unix/command.cpp)
elseif (WIN32)
    file(GLOB LINKER_SRC linker/windows/linker.cpp os/windows/command.cpp)
endif ()

add_executable(${PROJECT_NAME} main.cpp
        ${LINKER_SRC}
        compiler/Compiler.cpp
        compiler/backends/llvm_backend.cpp
        compiler/backends/llvm_backend.h
)
target_link_libraries(${PROJECT_NAME} PUBLIC parser ${llvm_libs})