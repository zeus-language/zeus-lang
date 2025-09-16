cmake_minimum_required(VERSION 3.16)
project(zeus-lsp)
#add_subdirectory(../parser parser_build)


add_executable(${PROJECT_NAME} main.cpp ${SRC}
)

target_link_libraries(${PROJECT_NAME} PUBLIC parser)