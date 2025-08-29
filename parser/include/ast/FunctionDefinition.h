#pragma once

#include <memory>
#include <vector>

#include "ASTNode.h"
#include "lexer/Lexer.h"


namespace ast {
    struct FunctionArgument {
        std::string name;
        std::string type; //TODO
    };

    class FunctionDefinition final : public ASTNode {
        std::vector<FunctionArgument> m_args;
        std::vector<std::unique_ptr<ASTNode> > m_statements;

    public:
        explicit FunctionDefinition(Token functionName, std::vector<FunctionArgument> args,
                                    std::vector<std::unique_ptr<ASTNode> > statements);

        ~FunctionDefinition() override = default;

        [[nodiscard]] std::string functionName() const;

        std::vector<FunctionArgument> &args();

        std::vector<std::unique_ptr<ASTNode> > &statements() { return m_statements; }

        FunctionDefinition(FunctionDefinition &&) = default;

        FunctionDefinition(const FunctionDefinition &) = delete;

        FunctionDefinition &operator=(FunctionDefinition &&) = delete;

        FunctionDefinition &operator=(const FunctionDefinition &) = delete;
    };
} // ast

