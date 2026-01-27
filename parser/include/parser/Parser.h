//
// Created by stefan on 29.08.25.
//

#ifndef ZEUS_LANG_PARSER_H
#define ZEUS_LANG_PARSER_H
#include <algorithm>
#include <vector>
#include <ranges>
#include <iostream>
#include "../lexer/Lexer.h"
#include "ast/ASTNode.h"

namespace ast {
    struct RawType;
    class FunctionDefinitionBase;
}

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

    struct Module;

    struct Module {
    private:
        std::vector<Token> m_modulePath;
        std::string modName;

    public:
        std::vector<std::shared_ptr<Module> > modules;
        std::vector<std::unique_ptr<ast::ASTNode> > nodes;
        std::vector<std::unique_ptr<ast::RawType> > externTypes;
        std::vector<std::unique_ptr<ast::FunctionDefinitionBase> > functions;

        std::vector<std::unique_ptr<ast::ASTNode> > useModuleNodes;

        std::optional<std::string> aliasName;
        void setModulePath(const std::vector<Token> &pathTokens) {
            m_modulePath = pathTokens;
            modName = "";
            for (auto &ns: m_modulePath) {
                modName += ns.lexical() + "::";
            }
        }
        const std::vector<Token>& modulePath() const {
            return m_modulePath;
        }

        [[nodiscard]] bool containsSubModule(const std::string &moduleName) const {
            for (const auto &mod: modules) {
                if (!mod->m_modulePath.empty() && mod->m_modulePath.back().lexical() == moduleName) {
                    return true;
                }
            }
            return false;
        }

        [[nodiscard]] std::string modulePathName() const {
            return modName;
        }

        [[nodiscard]] std::optional<std::pair<Module *, ast::FunctionDefinitionBase *> > findFunctionsByName(
            const std::string &path,
            const std::string &name) const;

        std::optional<std::pair<ast::ASTNode *, ast::ASTNode *> > getNodeByToken(const Token &token) const;

         [[nodiscard]] std::vector<std::shared_ptr<Module>> findModulesByPathStart(const std::vector<Token> &pathTokens) const;
        Module() = default;
        // Copy constructor
        Module(const Module &other) ;
    };


    struct ParseResult {
        std::shared_ptr<Module> module;
        std::vector<ParserMessasge> messages;

        [[nodiscard]] bool hasError() const {
            return std::ranges::any_of(
                messages, [](const auto &msg) { return msg.outputType == OutputType::ERROR; });
        }
    };

    ParseResult parse_tokens(const std::vector<Token> &tokens);
}

#endif //ZEUS_LANG_PARSER_H
