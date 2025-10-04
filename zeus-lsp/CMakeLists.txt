cmake_minimum_required(VERSION 3.16)
project(zeusls)
set(CMAKE_CXX_STANDARD 20)

#add_subdirectory(../parser parser_build)
include_directories(src)

file(GLOB SRC)
add_executable(${PROJECT_NAME} main.cpp ${SRC}
        src/lsp/LanguageServer.cpp
        src/lsp/CompilerOptions.cpp
        src/json/json.hpp
)

target_link_libraries(${PROJECT_NAME} PUBLIC parser)