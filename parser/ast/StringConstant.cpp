//
// Created by stefan on 29.08.25.
//

#include "ast/StringConstant.h"

namespace ast {
    StringConstant::StringConstant(Token constant)
        : ASTNode(std::move(constant)) {
    }

    std::string StringConstant::value() const {
        const auto val = expressionToken().lexical();
        std::string result;
        for (size_t i = 0; i < val.size(); ++i) {
            if (val[i] == '\\') {
                switch (val[i + 1]) {
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
            } else {
                result += val[i];
            }
        }
        return result;
    }
} // ast
