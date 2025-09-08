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
        include/ast/VariableDeclaration.h
        include/ast/LogicalExpression.h
        include/ast/VariableAssignment.h
        include/ast/BinaryExpression.h
        include/types/VariableType.h
        include/types/TypeRegistry.h
        include/types/TypeChecker.h
        include/ast/Comparisson.h
        include/ast/IfCondition.h
        include/ast/WhileLoop.h

        #include/parser/parser.h
)
target_sources(${PROJECT_NAME} PRIVATE
        Lexer.cpp
        Parser.cpp
        TypeRegistry.cpp
        TypeChecker.cpp
        ast/ASTNode.cpp
        ast/FunctionCallNode.cpp
        ast/ReturnStatement.cpp
        ast/StringConstant.cpp
        ast/NumberConstant.cpp
        ast/FunctionDefinition.cpp
        ast/VariableDeclaration.cpp
        ast/VariableAssignment.cpp)


target_include_directories(${PROJECT_NAME} PUBLIC include)