//
// Created by stefan on 29.08.25.
//

#pragma once

#include <memory>
#include <optional>

#include "ASTNode.h"
#include "lexer/Lexer.h"

namespace ast {
    class RawStringConstant final : public ASTNode {
    public:
        explicit RawStringConstant(Token constant);

        ~RawStringConstant() override = default;

        RawStringConstant(RawStringConstant &&) = default;

        RawStringConstant(const RawStringConstant &) = delete;

        RawStringConstant &operator=(RawStringConstant &&) = delete;

        RawStringConstant &operator=(const RawStringConstant &) = delete;

        [[nodiscard]] std::string value() const;

        [[nodiscard]] bool constant() const override;

        std::unique_ptr<ASTNode> clone() override {
            auto cloneNode = std::make_unique<RawStringConstant>(expressionToken());
            if (expressionType())
                cloneNode->setExpressionType(expressionType().value());
            return cloneNode;
        }
    };
} // ast


