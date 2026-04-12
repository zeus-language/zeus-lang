#pragma once
#include "ASTNode.h"
#include "FunctionDefinitionBase.h"

namespace ast {
    class LambdaExpression : public FunctionDefinitionBase {
        std::unique_ptr<BlockNode> m_blockNode;

    public:
        LambdaExpression(const Token &functionName, const std::vector<FunctionArgument> &args,
                         std::optional<std::unique_ptr<RawType> > returnType,
                         std::unique_ptr<BlockNode> blockNode,
                         std::vector<std::unique_ptr<RawAnnotation> > annotations)
            : FunctionDefinitionBase(functionName, args, std::move(returnType), std::move(annotations),
                                     VisibilityModifier::PRIVATE), m_blockNode(std::move(blockNode)) {
            setFunctionName(functionSignature(false));
        }


        [[nodiscard]] std::unique_ptr<ast::FunctionDefinitionBase> cloneFunction() override {
            auto returnTypeClone = returnType().has_value()
                                       ? std::make_optional<std::unique_ptr<RawType> >(returnType().value()->clone())
                                       : std::nullopt;
            auto block = m_blockNode->cloneBlock();
            std::vector<std::unique_ptr<RawAnnotation> > annotationsClones;
            for (const auto &annotation: rawAnnotations()) {
                annotationsClones.push_back(annotation->cloneAnnotation());
            }
            auto cloneNode = std::make_unique<LambdaExpression>(expressionToken(),
                                                                args(),
                                                                std::move(returnTypeClone),
                                                                std::move(block),
                                                                std::move(annotationsClones));
            if (expressionType())
                cloneNode->setExpressionType(expressionType().value());
            cloneNode->setModulePath(modulePath());
            return std::move(cloneNode);
        }

        std::unique_ptr<ASTNode> clone() override {
            return cloneFunction();
        }


        [[nodiscard]] BlockNode *block() const { return m_blockNode.get(); }

        [[nodiscard]] bool isMethod() const override {
            return false;
        }
    };
} // ast
