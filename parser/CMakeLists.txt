project("parser")
set(CMAKE_CXX_STANDARD 20)

set(ZEUS_VERSION_MAJOR 0)
set(ZEUS_VERSION_MINOR 1)
set(ZEUS_VERSION_PATCH 2)
add_compile_definitions(ZEUS_PATH="${ZEUS_PATH}")

file(WRITE "${PROJECT_BINARY_DIR}/VERSION.TXT" "${ZEUS_VERSION_MAJOR}.${ZEUS_VERSION_MINOR}.${ZEUS_VERSION_PATCH}")

# configure a header file to pass some of the CMake settings
# to the source code
configure_file(
        "${PROJECT_SOURCE_DIR}/config.h.in"
        "${PROJECT_BINARY_DIR}/config.h"
)

add_library(${PROJECT_NAME} STATIC
        Lexer.cpp
        Parser.cpp
        TypeRegistry.cpp
        TypeChecker.cpp
        Environment.cpp
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
        ast/BlockNode.cpp
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


target_include_directories(${PROJECT_NAME} PUBLIC include "${PROJECT_BINARY_DIR}")