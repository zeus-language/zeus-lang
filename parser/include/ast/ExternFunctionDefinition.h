#pragma once
#include "ASTNode.h"
#include "FunctionDefinition.h"

namespace ast {
    class ExternFunctionDefinition final : public FunctionDefinitionBase {
    public:
        explicit ExternFunctionDefinition(const Token &functionName, std::vector<FunctionArgument> args,
                                          std::optional<std::shared_ptr<RawType> > returnType,
                                          std::vector<std::shared_ptr<RawAnnotation> >
                                          annotations,
                                          const VisibilityModifier visibilityModifier) : FunctionDefinitionBase(
            functionName, std::move(args), std::move(returnType), std::move(annotations), visibilityModifier) {
        }

        ~ExternFunctionDefinition() override = default;


        ExternFunctionDefinition(ExternFunctionDefinition &&) = default;

        ExternFunctionDefinition(const ExternFunctionDefinition &) = delete;

        ExternFunctionDefinition &operator=(ExternFunctionDefinition &&) = delete;

        ExternFunctionDefinition &operator=(const ExternFunctionDefinition &) = delete;

        std::shared_ptr<ast::FunctionDefinitionBase> cloneFunction() override;

        [[nodiscard]] bool isMethod() const override;
    };
}
