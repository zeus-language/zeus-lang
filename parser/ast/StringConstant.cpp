//
// Created by stefan on 29.08.25.
//

#include "ast/StringConstant.h"

namespace ast {
    StringConstant::StringConstant(Token constant)
        : ASTNode(std::move(constant)) {
    }
} // ast
