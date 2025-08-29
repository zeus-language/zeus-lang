project("parser")
set(CMAKE_CXX_STANDARD 20)

add_library(${PROJECT_NAME} STATIC
        include/lexer/Lexer.h
        include/magic_enum/magic_enum.hpp
        include/parser/Parser.h
        include/ast/ASTNode.h
        include/ast/FunctionCallNode.h
        include/ast/ReturnStatement.h
        include/ast/StringConstant.h
        include/ast/NumberConstant.h
        include/ast/FunctionDefinition.h
        #include/parser/parser.h
)
target_sources(${PROJECT_NAME} PRIVATE
        Lexer.cpp
        Parser.cpp
        ast/ASTNode.cpp
        ast/FunctionCallNode.cpp
        ast/ReturnStatement.cpp
        ast/StringConstant.cpp
        ast/NumberConstant.cpp
        ast/FunctionDefinition.cpp)
        

target_include_directories(${PROJECT_NAME} PUBLIC include)