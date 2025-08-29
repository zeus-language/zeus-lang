//
// Created by stefan on 29.08.25.
//

#pragma once

#include <memory>
#include <optional>

#include "ASTNode.h"
#include "lexer/Lexer.h"

namespace ast {
    class StringConstant final : public ASTNode {
    public:
        explicit StringConstant(Token constant);

        ~StringConstant() override = default;

        StringConstant(StringConstant &&) = default;

        StringConstant(const StringConstant &) = delete;

        StringConstant &operator=(StringConstant &&) = delete;

        StringConstant &operator=(const StringConstant &) = delete;
    };
} // ast


