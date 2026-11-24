cmake_minimum_required(VERSION 3.16)
project(zeusls)
set(CMAKE_CXX_STANDARD 20)

include(../cmake/CPM.cmake)
CPMAddPackage(
        NAME lsp
        GIT_REPOSITORY "https://github.com/leon-bckl/lsp-framework"
        GIT_SUBMODULES ""
        GIT_TAG "db64fac3eea5b36888b801cc0c8b081a377f1914"
        #     OPTIONS "LSP_INSTALL Off"
)
file(GLOB SRC
        src/lsp/CompilerOptions.cpp
        src/lsp/LanguageServer.cpp
)

find_package(lsp 2.0.0 EXACT REQUIRED)
set(LSP_BUILD_DIR "${lsp_BINARY_DIR}/generated")
message("gen dir: ${LSP_GENERATED_FILES_DIR}")
message("gen dir: ${LSP_BUILD_DIR}")
add_executable(${PROJECT_NAME} main.cpp ${SRC})
# Make the headers in src available for includes
target_include_directories(${PROJECT_NAME} PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/src ${LSP_BUILD_DIR})

target_link_libraries(zeusls PRIVATE lsp parser)