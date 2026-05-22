//
// Created by stefan on 29.08.25.
//

#include "ast/RawStringConstant.h"

namespace ast {
    RawStringConstant::RawStringConstant(Token constant)
        : ASTNode(std::move(constant)) {
    }

    std::string RawStringConstant::value() const {
        const auto lexical = expressionToken().lexical().substr(2, expressionToken().lexical().size() - 3);
        std::string result;
        for (size_t i = 0; i < lexical.size(); ++i) {
            if (lexical[i] == '\\') {
                switch (lexical[i + 1]) {
                    case 'n':
                        result += '\n';
                        break;
                    case 't':
                        result += '\t';
                        break;
                    case 'r':
                        result += '\r';
                        break;
                    case '\\':
                        result += '\\';
                        break;
                    case '\'':
                        result += '\'';
                        break;
                    case '\"':
                        result += '\"';
                        break;
                    case 'u':
                        while (i + 1 < lexical.size() && lexical[i + 1] != '{')
                            i++;
                        if (i + 1 < lexical.size() && lexical[i + 1] == '{') {
                            i += 2;
                            std::string hexCode;
                            while (i < lexical.size() && lexical[i] != '}') {
                                hexCode += lexical[i];
                                i++;
                            }
                            if (i < lexical.size() && lexical[i] == '}') {
                                char32_t codePoint = std::stoul(hexCode, nullptr, 16);
                                if (codePoint <= 0x7F) {
                                    result += static_cast<char>(codePoint);
                                } else if (codePoint <= 0x7FF) {
                                    result += static_cast<char>(0xC0 | ((codePoint >> 6) & 0x1F));
                                    result += static_cast<char>(0x80 | (codePoint & 0x3F));
                                } else if (codePoint <= 0xFFFF) {
                                    result += static_cast<char>(0xE0 | ((codePoint >> 12) & 0x0F));
                                    result += static_cast<char>(0x80 | ((codePoint >> 6) & 0x3F));
                                    result += static_cast<char>(0x80 | (codePoint & 0x3F));
                                } else if (codePoint <= 0x10FFFF) {
                                    result += static_cast<char>(0xF0 | ((codePoint >> 18) & 0x07));
                                    result += static_cast<char>(0x80 | ((codePoint >> 12) & 0x3F));
                                    result += static_cast<char>(0x80 | ((codePoint >> 6) & 0x3F));
                                    result += static_cast<char>(0x80 | (codePoint & 0x3F));
                                }
                            }
                        }
                        break;
                    default:
                        break;
                }
                i++;
            } else {
                result += lexical[i];
            }
        }
        return result;
    }

    bool RawStringConstant::constant() const {
        return true;
    }
} // ast
