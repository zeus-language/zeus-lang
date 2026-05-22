#include "ast/VariableAccess.h"

#include <iostream>
#include <ostream>

static int numberOfVariableAccesses = 0;


ast::VariableAccess::VariableAccess(Token name) : ASTNode(std::move(name), NodeType::VARIABLE_ACCESS) {
    numberOfVariableAccesses++;
}

ast::VariableAccess::~VariableAccess() {
    numberOfVariableAccesses--;
    //    std::cerr << "VariableAccess destroyed, remaining: " << numberOfVariableAccesses << std::endl;
}
