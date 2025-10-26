cmake_minimum_required(VERSION 3.16)
project(zeusls)
set(CMAKE_CXX_STANDARD 20)

include(../cmake/CPM.cmake)
CPMAddPackage(
        NAME lsp-framework
        GITHUB_REPOSITORY leon-bckl/lsp-framework
        GIT_TAG 1.2.0
)
# Quellen rekursiv sammeln (nur cpp/c)
file(GLOB_RECURSE SRC CONFIGURE_DEPENDS "src/*.cpp" "src/*.c" "src/**/*.cpp" "src/**/*.c")

add_executable(${PROJECT_NAME} main.cpp ${SRC})

# Make the headers in src available for includes
target_include_directories(${PROJECT_NAME} PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/src)

# lsp-framework headers are provided by CPM in the build _deps directory; expose those include dirs
# Check multiple likely locations (current build dir, sibling build dirs)
set(_lsp_candidate_dirs
        "${CMAKE_BINARY_DIR}/_deps/lsp-framework-src"
        "${CMAKE_BINARY_DIR}/_deps/lsp-framework-build/generated"
        "${CMAKE_BINARY_DIR}/_deps/lsp-framework-build"
        "${CMAKE_SOURCE_DIR}/cmake-build-debug/_deps/lsp-framework-src"
        "${CMAKE_SOURCE_DIR}/cmake-build-debug/_deps/lsp-framework-build/generated"
        "${CMAKE_SOURCE_DIR}/cmake-build-release/_deps/lsp-framework-src"
        "${CMAKE_SOURCE_DIR}/cmake-build-release/_deps/lsp-framework-build/generated"
)

foreach (_dir IN LISTS _lsp_candidate_dirs)
    if (EXISTS "${_dir}")
        message(STATUS "Adding lsp-framework include dir: ${_dir}")
        target_include_directories(${PROJECT_NAME} PRIVATE ${_dir})
    endif ()
endforeach ()

# Also ensure the generated headers subdirectory (common name) is visible if created elsewhere
if (EXISTS "${CMAKE_BINARY_DIR}/generated/lsp")
    target_include_directories(${PROJECT_NAME} PRIVATE ${CMAKE_BINARY_DIR}/generated)
endif ()

# Link against the parser target and lsp library (if available)
# If 'lsp' target exists, linking will propagate include dirs; if not, we still added include dirs above.
find_package(lsp CONFIG QUIET)
# It's ok if find_package fails; we still try to link the target in case CPM created it as an imported target
if (TARGET lsp)
    target_link_libraries(${PROJECT_NAME} PUBLIC parser lsp)
else ()
    target_link_libraries(${PROJECT_NAME} PUBLIC parser)
endif ()

# If the lsp target is not available yet, try to add the CPM-downloaded lsp-framework as a subdirectory
if (NOT TARGET lsp)
    if (EXISTS "${CMAKE_BINARY_DIR}/_deps/lsp-framework-src/CMakeLists.txt")
        message(STATUS "Adding lsp-framework subdirectory from: ${CMAKE_BINARY_DIR}/_deps/lsp-framework-src")
        add_subdirectory("${CMAKE_BINARY_DIR}/_deps/lsp-framework-src" "${CMAKE_BINARY_DIR}/_deps/lsp-framework-build" EXCLUDE_FROM_ALL)
    elseif (EXISTS "${CMAKE_SOURCE_DIR}/cmake-build-debug/_deps/lsp-framework-src/CMakeLists.txt")
        message(STATUS "Adding lsp-framework subdirectory from: ${CMAKE_SOURCE_DIR}/cmake-build-debug/_deps/lsp-framework-src")
        add_subdirectory("${CMAKE_SOURCE_DIR}/cmake-build-debug/_deps/lsp-framework-src" "${CMAKE_SOURCE_DIR}/cmake-build-debug/_deps/lsp-framework-build" EXCLUDE_FROM_ALL)
    endif ()
endif ()

# Now link against parser and lsp (if available)
if (TARGET lsp)
    target_link_libraries(${PROJECT_NAME} PUBLIC parser lsp)
else ()
    target_link_libraries(${PROJECT_NAME} PUBLIC parser)
endif ()
