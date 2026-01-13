cmake_minimum_required(VERSION 3.16)
project(zeusls)
set(CMAKE_CXX_STANDARD 20)

include(../cmake/CPM.cmake)
CPMAddPackage(
        NAME lsp
        GIT_REPOSITORY "https://github.com/leon-bckl/lsp-framework"
        GIT_TAG "1.3.0"
)
file(GLOB SRC
        src/lsp/CompilerOptions.cpp
        src/lsp/LanguageServer.cpp
)

find_package(lsp 1.3.0 EXACT REQUIRED)


add_executable(${PROJECT_NAME} main.cpp ${SRC})

# Make the headers in src available for includes
target_include_directories(${PROJECT_NAME} PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/src)

target_link_libraries(zeusls PUBLIC lsp parser)