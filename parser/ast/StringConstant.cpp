//
// Created by stefan on 29.08.25.
//

#include "ast/StringConstant.h"

namespace ast {
    StringConstant::StringConstant(Token constant)
        : ASTNode(std::move(constant)) {
    }

    std::string StringConstant::value() const {
        auto lexical = expressionToken().lexical();

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
                    default:
                        break;
                }
                i++;
            } else if (lexical[i] != '"') {
                result += lexical[i];
            }
        }
        return result;
    }

    bool StringConstant::constant() const {
        return true;
    }
} // ast
