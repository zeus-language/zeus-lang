project("parser")
set(CMAKE_CXX_STANDARD 20)

add_library(${PROJECT_NAME} STATIC
        Lexer.cpp
        Parser.cpp
        TypeRegistry.cpp
        TypeChecker.cpp
        ast/ASTNode.cpp
        ast/FunctionCallNode.cpp
        ast/ReturnStatement.cpp
        ast/StringConstant.cpp
        ast/RawStringConstant.cpp
        ast/NumberConstant.cpp
        ast/FunctionDefinition.cpp
        ast/ExternFunctionDefinition.cpp
        ast/VariableDeclaration.cpp
        ast/VariableAssignment.cpp
        include/parser/module.h
        module.cpp

        include/ast/RangeExpression.h
        include/ast/EnumDeclaration.h
        include/ast/EnumAccess.h
        Scope.cpp
        include/types/Scope.h
        VariableType.cpp
        include/ast/RawAnnotation.h
        include/types/Annotation.h
        ast/AnnotatedNode.cpp
        include/ast/AnnotatedNode.h

)
target_sources(${PROJECT_NAME} PRIVATE
)


target_include_directories(${PROJECT_NAME} PUBLIC include)