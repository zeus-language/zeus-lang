#pragma once
#include "ASTNode.h"
#include "FunctionDefinition.h"

namespace ast {
    class ExternFunctionDefinition final : public FunctionDefinitionBase {
    public:
        explicit ExternFunctionDefinition(Token functionName, std::vector<FunctionArgument> args,
                                          std::optional<std::unique_ptr<RawType> > returnType) : FunctionDefinitionBase(
            std::move(functionName), std::move(args), std::move(returnType)) {
        }

        ~ExternFunctionDefinition() override = default;


        ExternFunctionDefinition(ExternFunctionDefinition &&) = default;

        ExternFunctionDefinition(const ExternFunctionDefinition &) = delete;

        ExternFunctionDefinition &operator=(ExternFunctionDefinition &&) = delete;

        ExternFunctionDefinition &operator=(const ExternFunctionDefinition &) = delete;
    };
}
