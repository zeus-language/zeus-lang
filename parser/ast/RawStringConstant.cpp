//
// Created by stefan on 29.08.25.
//

#include "ast/RawStringConstant.h"

namespace ast {
    RawStringConstant::RawStringConstant(Token constant)
        : ASTNode(std::move(constant)) {
    }

    std::string RawStringConstant::value() const {
        const auto val = expressionToken().lexical().substr(2, expressionToken().lexical().size() - 3);
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

    bool RawStringConstant::constant() const {
        return true;
    }
} // ast
