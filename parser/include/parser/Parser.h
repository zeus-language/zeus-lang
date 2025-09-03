//
// Created by stefan on 29.08.25.
//

#ifndef ZEUS_LANG_PARSER_H
#define ZEUS_LANG_PARSER_H
#include <algorithm>
#include <vector>
#include <utility>
#include "../lexer/Lexer.h"
#include "ast/ASTNode.h"

namespace parser {
    enum class OutputType {
        ERROR,
        WARN,
        HINT
    };

    namespace Color {
        enum Code :uint16_t {
            FG_RED = 31,
            FG_GREEN = 32,
            FG_BLUE = 34,
            FG_DEFAULT = 39,
            BG_RED = 41,
            BG_GREEN = 42,
            BG_BLUE = 44,
            BG_DEFAULT = 49
        };

        class Modifier {
            Code code;

        public:
            Modifier(Code pCode) : code(pCode) {
            }

            friend std::ostream &operator<<(std::ostream &os, const Modifier &mod) {
                return os << "\033[" << static_cast<uint16_t>(mod.code) << "m";
            }
        };
    } // namespace Color


    struct ParserMessasge {
        OutputType outputType = OutputType::ERROR;
        Token token;
        std::string message;

        void msg(std::ostream &ostream, bool printColor) const;
    };

    struct ParseResult {
        std::vector<std::unique_ptr<ast::ASTNode> > nodes;
        std::vector<ParserMessasge> messages;

        [[nodiscard]] bool hasError() const {
            return std::ranges::any_of(
                messages, [](const auto &msg) { return msg.outputType == OutputType::ERROR; });
        }
    };

    ParseResult parse_tokens(const std::vector<Token> &tokens);
}

#endif //ZEUS_LANG_PARSER_H
