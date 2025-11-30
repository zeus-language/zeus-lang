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

        [[nodiscard]] std::string value() const;

        [[nodiscard]] bool constant() const override;

        std::unique_ptr<ASTNode> clone() override {
            auto cloneNode = std::make_unique<StringConstant>(expressionToken());
            if (expressionType())
                cloneNode->setExpressionType(expressionType().value());
            return cloneNode;
        }
    };
} // ast


