#pragma once
#include "ASTNode.h"
#include "FunctionDefinitionBase.h"

namespace ast {
    class LambdaExpression : public FunctionDefinitionBase {
        std::shared_ptr<BlockNode> m_blockNode;

    public:
        LambdaExpression(const Token &functionName, const std::vector<FunctionArgument> &args,
                         std::optional<std::shared_ptr<RawType> > returnType,
                         std::shared_ptr<BlockNode> blockNode,
                         std::vector<std::shared_ptr<RawAnnotation> > annotations)
            : FunctionDefinitionBase(functionName, args, std::move(returnType), std::move(annotations),
                                     VisibilityModifier::PRIVATE), m_blockNode(std::move(blockNode)) {
            setFunctionName("fn");
        }


        [[nodiscard]] std::shared_ptr<ast::FunctionDefinitionBase> cloneFunction() override {
            auto returnTypeClone = returnType().has_value()
                                       ? std::make_optional<std::shared_ptr<RawType> >(returnType().value()->clone())
                                       : std::nullopt;
            auto block = m_blockNode->cloneBlock();
            std::vector<std::shared_ptr<RawAnnotation> > annotationsClones;
            for (const auto &annotation: rawAnnotations()) {
                annotationsClones.push_back(annotation->cloneAnnotation());
            }
            auto cloneNode = std::make_shared<LambdaExpression>(expressionToken(),
                                                                args(),
                                                                std::move(returnTypeClone),
                                                                std::move(block),
                                                                std::move(annotationsClones));
            if (expressionType())
                cloneNode->setExpressionType(expressionType().value());
            if (resolvedReturnType())
                cloneNode->setResolvedReturnType(resolvedReturnType().value());
            cloneNode->setModulePath(modulePath());
            return std::move(cloneNode);
        }

        std::shared_ptr<ASTNode> clone() override {
            return cloneFunction();
        }


        [[nodiscard]] BlockNode *block() const { return m_blockNode.get(); }

        [[nodiscard]] bool isMethod() const override {
            return false;
        }
    };
} // ast
