#include "types/TypeChecker.h"

#include <cassert>
#include <map>

#include "ast/BinaryExpression.h"
#include "ast/FunctionCallNode.h"
#include "ast/FunctionDefinition.h"
#include "ast/NumberConstant.h"
#include "ast/ReturnStatement.h"
#include "ast/StringConstant.h"
#include "ast/VariableAccess.h"
#include "ast/VariableAssignment.h"
#include "ast/VariableDeclaration.h"
#include "types/TypeRegistry.h"

namespace types {
    struct Variable {
        std::string name;
        std::shared_ptr<VariableType> type;
        bool constant;
    };

    struct Context {
        TypeRegistry registry;
        std::map<std::string, std::optional<Variable> > currentVariables;
        std::vector<parser::ParserMessasge> messages;
    };

    void type_check(ast::FunctionDefinition *node, Context &context);

    void type_check(ast::FunctionCallNode *node, Context &context);

    void type_check(ast::VariableAssignment *node, Context &context);

    void type_check(ast::VariableAccess *node, Context &context);

    void type_check(ast::VariableDeclaration *node, Context &context);

    void type_check(ast::ReturnStatement *node, Context &context);

    void type_check(ast::BinaryExpression *node, Context &context);

    void type_check(ast::StringConstant *node, Context &context) {
        node->setExpressionType(context.registry.getTypeByName("string").value());
    }

    void type_check(ast::NumberConstant *node, Context &context) {
        switch (node->numberType()) {
            case ast::NumberType::INTEGER:
                node->setExpressionType(context.registry.getTypeByName("i32").value());
                break;
            case ast::NumberType::FLOAT:
                node->setExpressionType(context.registry.getTypeByName("float").value());
                break;
            default:
                assert(false && "Unknown number type");
        }
    }


    void type_check_base(ast::ASTNode *node, Context &context) {
        if (const auto funcDef = dynamic_cast<ast::FunctionDefinition *>(node)) {
            return type_check(funcDef, context);
        }
        if (const auto varAssign = dynamic_cast<ast::VariableAssignment *>(node)) {
            return type_check(varAssign, context);
        }
        if (const auto varDecl = dynamic_cast<ast::VariableDeclaration *>(node)) {
            return type_check(varDecl, context);
        }
        if (const auto returnStmt = dynamic_cast<ast::ReturnStatement *>(node)) {
            return type_check(returnStmt, context);
        }
        if (const auto funcCall = dynamic_cast<ast::FunctionCallNode *>(node)) {
            return type_check(funcCall, context);
        }
        if (const auto number = dynamic_cast<ast::NumberConstant *>(node)) {
            return type_check(number, context);
        }
        if (const auto string = dynamic_cast<ast::StringConstant *>(node)) {
            return type_check(string, context);
        }
        if (const auto binExpr = dynamic_cast<ast::BinaryExpression *>(node)) {
            return type_check(binExpr, context);
        }
        if (const auto varAccess = dynamic_cast<ast::VariableAccess *>(node)) {
            return type_check(varAccess, context);
        }


        context.messages.push_back({
            parser::OutputType::ERROR,
            node->expressionToken(),
            "Unknown AST node that can not be type checked yet."
        });
    }

    void type_check(ast::VariableAccess *node, Context &context) {
        const auto it = context.currentVariables.find(node->expressionToken().lexical());
        if (it == context.currentVariables.end()) {
            context.messages.push_back({
                parser::OutputType::ERROR,
                node->expressionToken(),
                "Variable '" + node->expressionToken().lexical() + "' is not declared in this scope."
            });
            return;
        }
        if (it->second.has_value()) {
            node->setExpressionType(it->second->type);
        } else {
            context.messages.push_back({
                parser::OutputType::ERROR,
                node->expressionToken(),
                "Variable '" + node->expressionToken().lexical() + "' has no type information."
            });
        }
    }

    void type_check(ast::BinaryExpression *node, Context &context) {
        type_check_base(node->lhs(), context);
        type_check_base(node->rhs(), context);

        if (node->lhs()->expressionType() && node->rhs()->expressionType()) {
            if (node->lhs()->expressionType().value()->name() != node->rhs()->expressionType().value()->name()) {
                context.messages.push_back({
                    parser::OutputType::ERROR,
                    node->expressionToken(),
                    "Type mismatch in binary expression: left is of type '" +
                    node->lhs()->expressionType().value()->name() + "', right is of type '" +
                    node->rhs()->expressionType().value()->name() + "'."
                });
                return;
            }
            // For simplicity, we assume the result type is the same as the operand types
            node->setExpressionType(node->lhs()->expressionType().value());
        } else {
            context.messages.push_back({
                parser::OutputType::ERROR,
                node->expressionToken(),
                "Could not determine types of operands in binary expression."
            });
        }
        // Further type checking logic would go here...
    }

    void type_check(ast::FunctionCallNode *node, Context &context) {
        for (auto &arg: node->args()) {
            type_check_base(arg.get(), context);
        }
        // Further type checking logic would go here...
    }

    void type_check(ast::ReturnStatement *node, Context &context) {
        if (node->returnValue()) {
            type_check_base(node->returnValue().value(), context);
        }
        // Further type checking logic would go here...
    }

    void type_check(ast::VariableDeclaration *node, Context &context) {
        auto varType = context.registry.getTypeByName(node->type().lexical());
        if (!varType) {
            context.messages.push_back({
                parser::OutputType::ERROR,
                node->expressionToken(),
                "Unknown type '" + node->type().lexical() + "' for variable '" + node->expressionToken().lexical() +
                "'."
            });
            return;
        } else {
            node->setExpressionType(varType.value());
        }
        if (context.currentVariables.find(node->expressionToken().lexical()) != context.currentVariables.end()) {
            context.messages.push_back({
                parser::OutputType::ERROR,
                node->expressionToken(),
                "Variable '" + node->expressionToken().lexical() + "' is already declared in this scope."
            });
            return;
        }
        context.currentVariables.emplace(node->expressionToken().lexical(),
                                         Variable{
                                             node->expressionToken().lexical(), varType.value_or(nullptr),
                                             node->constant()
                                         });
        if (node->initialValue()) {
            type_check_base(node->initialValue().value(), context);
            // check whenever the types match
        }
    }


    void type_check(ast::VariableAssignment *node, Context &context) {
        type_check_base(node->expression(), context);
        // check whenever the variable exists
        const auto it = context.currentVariables.find(node->expressionToken().lexical());
        if (it == context.currentVariables.end()) {
            context.messages.push_back({
                parser::OutputType::ERROR,
                node->expressionToken(),
                "Variable '" + node->expressionToken().lexical() + "' is not declared in this scope."
            });
            return;
        }
        if (it->second.has_value() && it->second->constant) {
            context.messages.push_back({
                parser::OutputType::ERROR,
                node->expressionToken(),
                "Cannot assign to constant variable '" + node->expressionToken().lexical() + "'."
            });
            return;
        }
        // check whenever the types match
    }

    void type_check(ast::FunctionDefinition *node, Context &context) {
        // Example type checking logic for a function definition
        if (node->functionName() == "main" && !node->args().empty()) {
            context.messages.push_back({
                parser::OutputType::ERROR,
                node->expressionToken(),
                "The 'main' function must not have parameters."
            });
        }

        for (auto &arg: node->args()) {
            arg.type = context.registry.getTypeByName(arg.typeName);
            if (!arg.type) {
                context.messages.push_back({
                    parser::OutputType::ERROR,
                    node->expressionToken(),
                    "Unknown type '" + arg.typeName + "' for argument '" + arg.name + "'."
                });
            }
            context.currentVariables.emplace(arg.name, Variable{arg.name, arg.type.value_or(nullptr), false});
        }
        for (auto &stmt: node->statements()) {
            type_check_base(stmt.get(), context);
        }
        context.currentVariables.clear();
        // Further type checking logic would go here...
    }

    void type_check(const std::vector<std::unique_ptr<ast::ASTNode> > &nodes, TypeCheckResult &result) {
        Context context;
        for (auto &node: nodes) {
            type_check_base(node.get(), context);
        }
        result.messages = std::move(context.messages);
    }
}
