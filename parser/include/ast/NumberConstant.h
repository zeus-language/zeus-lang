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
        FLOAT
    };

    typedef std::variant<int64_t, double> NumberValue;

    NumberValue parseNumber(const std::string &lexical, NumberType type);

    class NumberConstant final : public ASTNode {
        NumberValue m_value;
        NumberType m_numberType;

    public:
        explicit NumberConstant(Token constant, NumberType numberType);

        NumberType numberType() const { return m_numberType; }
        NumberValue value() const { return m_value; }

        ~NumberConstant() override = default;

        NumberConstant(NumberConstant &&) = default;

        NumberConstant(const NumberConstant &) = delete;

        NumberConstant &operator=(NumberConstant &&) = delete;

        NumberConstant &operator=(const NumberConstant &) = delete;
    };
} // ast


