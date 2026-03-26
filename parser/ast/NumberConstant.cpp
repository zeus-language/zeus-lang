//
// Created by stefan on 29.08.25.
//

#include "ast/NumberConstant.h"

#include <cassert>
#include <cmath>

namespace ast {
    NumberValue parseNumber(const std::string &lexical, const NumberType type) {
        switch (type) {
            case NumberType::INTEGER:
                return static_cast<int64_t>(std::stoull(lexical));
            case NumberType::BIN_NUMBER:
                return static_cast<uint64_t>(std::stoull(lexical.substr(2), nullptr, 2));
            case NumberType::OCT_NUMBER:
                return static_cast<uint64_t>(std::stoull(lexical.substr(2), nullptr, 8));
            case NumberType::HEX_NUMBER:
                return static_cast<uint64_t>(std::stoull(lexical.substr(2), nullptr, 16));
            case NumberType::FLOAT:
                return std::stof(lexical);
            case NumberType::DOUBLE:
                return std::stod(lexical);
            case NumberType::BOOLEAN:
                return static_cast<bool>(lexical == "true");
            case NumberType::NULLPTR:
                return static_cast<int64_t>(0);
            case NumberType::CHAR:
                if (lexical.at(0) == '\'' && lexical.back() == '\'') {
                    // remove quotes
                    auto content = lexical.substr(1, lexical.size() - 2);
                    if (content.front() == '\\') {
                        switch (content[1]) {
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
                        return static_cast<int64_t>(content.front());
                    }
                }
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

    size_t NumberConstant::numBits() const {
        if (m_numberType == NumberType::FLOAT) {
            return 32;
        }
        if (m_numberType == NumberType::DOUBLE) {
            return 64;
        }
        if (m_numberType == NumberType::BOOLEAN) {
            return 1;
        }
        if (m_numberType == NumberType::CHAR) {
            return 8;
        }
        if (std::holds_alternative<uint64_t>(m_value)) {
            auto base = 1 + static_cast<int>(std::log2(std::get<uint64_t>(m_value)));
            base = (base > 32) ? 64 : 32;
            return base;
        }
        auto base = 1 + static_cast<int>(std::log2(std::get<int64_t>(m_value)));
        base = (base > 32) ? 64 : 32;
        return base;
    }
} // ast
