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
            case NumberType::CHAR:
                if (lexical.front() == '\\') {
                    switch (lexical[1]) {
                        case 'n':
                            return static_cast<int64_t>('\n');
                        case 't':
                            return static_cast<int64_t>('\t');
                        case 'r':
                            return static_cast<int64_t>('\r');
                        case '\\':
                            return static_cast<int64_t>('\\');
                        case '\'':
                            return static_cast<int64_t>('\'');
                        case '\"':
                            return static_cast<int64_t>('\"');
                        default:
                            assert(false && "unknown escape sequence");
                            return 0;
                    }
                } else {
                    return static_cast<int64_t>(lexical.front());
                }
        }
        assert(false && "unknown number type");
        return 0;
    }

    NumberConstant::NumberConstant(Token constant, const NumberType numberType)
        : ASTNode(std::move(constant)), m_value(parseNumber(expressionToken().lexical(), numberType)),
          m_numberType(numberType) {
    }
} // ast
