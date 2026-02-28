#pragma once
#include "ASTNode.h"
#include "FunctionDefinition.h"

namespace ast {
    class ExternFunctionDefinition final : public FunctionDefinitionBase {
    public:
        explicit ExternFunctionDefinition(const Token& functionName, std::vector<FunctionArgument> args,
                                          std::optional<std::unique_ptr<RawType> > returnType,
                                          std::vector<std::unique_ptr<RawAnnotation> >
                                          annotations,
                                        const VisibilityModifier visibilityModifier) : FunctionDefinitionBase(
            functionName, std::move(args), std::move(returnType), std::move(annotations),visibilityModifier) {
        }

        ~ExternFunctionDefinition() override = default;


        ExternFunctionDefinition(ExternFunctionDefinition &&) = default;

        ExternFunctionDefinition(const ExternFunctionDefinition &) = delete;

        ExternFunctionDefinition &operator=(ExternFunctionDefinition &&) = delete;

        ExternFunctionDefinition &operator=(const ExternFunctionDefinition &) = delete;

        std::unique_ptr<ast::FunctionDefinitionBase> cloneFunction() override;

    };
}
