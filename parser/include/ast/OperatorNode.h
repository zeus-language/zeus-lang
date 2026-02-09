#pragma once
#include <memory>
#include <optional>

#include "ASTNode.h"


namespace  ast {
    class FunctionDefinitionBase;

    class OperatorNode : public ASTNode {
    protected:
        std::unique_ptr<ASTNode> m_lhs;
        std::unique_ptr<ASTNode> m_rhs;
        std::optional<ast::FunctionDefinitionBase*> m_operatorFunction = std::nullopt;

    public:
         explicit OperatorNode(Token name, std::unique_ptr<ASTNode> lhs, std::unique_ptr<ASTNode> rhs) : ASTNode(std::move(name)),
                                                                                   m_lhs(std::move(lhs)),
                                                                                   m_rhs(std::move(rhs)) {
        }

        ~OperatorNode() override = default;
        [[nodiscard]] ASTNode *lhs() const { return m_lhs.get(); }
        [[nodiscard]] std::unique_ptr<ASTNode> movelhs() { return std::move(m_lhs); }
        [[nodiscard]] ASTNode *rhs() const { return m_rhs.get(); }
        void setLhs(std::unique_ptr<ASTNode> lhs) { m_lhs = std::move(lhs); }
        void setRhs(std::unique_ptr<ASTNode> rhs) { m_rhs = std::move(rhs);}

        [[nodiscard]] virtual   std::string operatorFunctionName() const = 0;
        void setOperatorFunction( ast::FunctionDefinitionBase* funcDef)  {
            m_operatorFunction = std::make_optional<ast::FunctionDefinitionBase*>(funcDef);
        }

        [[nodiscard]] std::optional<ast::FunctionDefinitionBase*> operatorFunction() const  {
            return m_operatorFunction;
        }


    };
}
