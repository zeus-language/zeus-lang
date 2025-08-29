//
// Created by stefan on 29.08.25.
//

#include "ast/NumberConstant.h"

#include <cassert>

namespace ast {
    NumberValue parseNumber(const std::string &lexical, const NumberType type) {
        switch (type) {
            case NumberType::INTEGER:
                return static_cast<int64_t>(std::stoull(lexical));
            case NumberType::FLOAT:
                return std::stod(lexical);
        }
        assert(false && "unknown number type");
        return 0;
    }

    NumberConstant::NumberConstant(Token constant, const NumberType numberType)
        : ASTNode(std::move(constant)), m_value(parseNumber(expressionToken().lexical(), numberType)),
          m_numberType(numberType) {
    }
} // ast
