//
// Created by stefan on 29.08.25.
//

#pragma once

#include <memory>
#include <optional>
#include <variant>

#include "ASTNode.h"
#include "lexer/Lexer.h"

namespace ast {
    enum class NumberType {
        INTEGER,
        FLOAT,
        CHAR,
        BOOLEAN
    };

    typedef std::variant<int64_t, double, bool> NumberValue;

    NumberValue parseNumber(const std::string &lexical, NumberType type);

    class NumberConstant final : public ASTNode {
        NumberValue m_value;
        NumberType m_numberType;

    public:
        explicit NumberConstant(Token constant, NumberType numberType);

        [[nodiscard]] NumberType numberType() const { return m_numberType; }
        [[nodiscard]] NumberValue value() const { return m_value; }

        [[nodiscard]] bool constant() const override {
            return true;
        }


        ~NumberConstant() override = default;

        size_t numBits() const;

        NumberConstant(NumberConstant &&) = default;

        NumberConstant(const NumberConstant &) = delete;

        NumberConstant &operator=(NumberConstant &&) = delete;

        NumberConstant &operator=(const NumberConstant &) = delete;
    };
} // ast


