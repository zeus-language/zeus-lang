cmake_minimum_required(VERSION 3.10)
project(zeus)

add_subdirectory(parser)

set(CMAKE_CXX_STANDARD 20)


find_package(LLVM 18.1.0 REQUIRED CONFIG)

message(STATUS " Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")
message(STATUS "Compiler: ${CMAKE_CXX_COMPILER_ID}")
include_directories(${LLVM_INCLUDE_DIRS} src)
add_definitions(${LLVM_DEFINITIONS})
# Find and link LLD libraries
llvm_map_components_to_libnames(llvm_libs support core irreader native nativecodegen passes)
include(FetchContent)
if (UNIX)
    # G++
    file(GLOB LINKER_SRC src/linker/unix/linker.cpp src/os/unix/command.cpp)
elseif (WIN32)
    file(GLOB LINKER_SRC src/linker/windows/linker.cpp src/os/windows/command.cpp)
endif ()

file(GLOB SRC
        ${LINKER_SRC}
        src/compiler/Compiler.cpp
        src/compiler/backends/llvm_backend.cpp
        src/compiler/backends/llvm_backend.h
        src/compiler/backends/llvm_intrinsics.h
        src/compiler/backends/llvm_intrinsics.cpp)
option(BUILD_TESTS "Build test programs" ON)
if (BUILD_TESTS)
    FetchContent_Declare(
            googletest
            URL https://github.com/google/googletest/archive/refs/tags/v1.15.0.zip
    )
    # For Windows: Prevent overriding the parent project's compiler/linker settings
    set(gtest_force_shared_crt ON CACHE BOOL "" FORCE)
    FetchContent_MakeAvailable(googletest)
    enable_testing()
    add_subdirectory(tests)

endif ()


if (${CMAKE_CXX_COMPILER_ID} STREQUAL "MSVC")
    set_property(TARGET ${PROJECT_NAME} PROPERTY
            MSVC_RUNTIME_LIBRARY "MultiThreaded$<$<CONFIG:Debug>:Debug>")
endif ()


add_executable(${PROJECT_NAME} main.cpp ${SRC}
        src/compiler/CompilerOptions.cpp
)
target_link_libraries(${PROJECT_NAME} PUBLIC parser ${llvm_libs})