#pragma once
#include "ASTNode.h"
#include "FunctionDefinition.h"

namespace ast {
    class FunctionSignature final : public FunctionDefinitionBase {
    public:
        explicit FunctionSignature(const Token &functionName, std::vector<FunctionArgument> args,
                                   std::optional<std::shared_ptr<RawType> > returnType,
                                   std::vector<std::shared_ptr<RawAnnotation> >
                                   annotations) : FunctionDefinitionBase(
            functionName, std::move(args), std::move(returnType), std::move(annotations), VisibilityModifier::PUBLIC) {
        }

        ~FunctionSignature() override = default;

        std::shared_ptr<FunctionSignature> cloneSignature();


        FunctionSignature(FunctionSignature &&) = default;

        FunctionSignature(const FunctionSignature &) = delete;

        FunctionSignature &operator=(FunctionSignature &&) = delete;

        FunctionSignature &operator=(const FunctionSignature &) = delete;

        std::shared_ptr<ast::FunctionDefinitionBase> cloneFunction() override;

        [[nodiscard]] bool isMethod() const override;
    };
}
