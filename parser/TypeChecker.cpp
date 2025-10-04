#include "types/TypeChecker.h"

#include <cassert>
#include <map>

#include "ast/ArrayAccess.h"
#include "ast/ArrayAssignment.h"
#include "ast/ArrayInitializer.h"
#include "ast/BinaryExpression.h"
#include "ast/BreakStatement.h"
#include "ast/Comparisson.h"
#include "ast/FieldAccess.h"
#include "ast/FieldAssignment.h"
#include "ast/ForLoop.h"
#include "ast/FunctionCallNode.h"
#include "ast/FunctionDefinition.h"
#include "ast/IfCondition.h"
#include "ast/LogicalExpression.h"
#include "ast/NumberConstant.h"
#include "ast/ReturnStatement.h"
#include "ast/StringConstant.h"
#include "ast/StructDeclaration.h"
#include "ast/StructInitialization.h"
#include "ast/TypeCast.h"
#include "ast/VariableAccess.h"
#include "ast/VariableAssignment.h"
#include "ast/VariableDeclaration.h"
#include "ast/WhileLoop.h"
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
        std::vector<ast::FunctionDefinition *> functions;
    };


    std::optional<std::shared_ptr<VariableType> > resolveFromRawType(ast::RawType *rawType, TypeRegistry &registry) {
        if (rawType->isPointer) {
            auto baseType = registry.getTypeByName(rawType->typeToken.lexical());
            if (!baseType) return std::nullopt;
            return types::TypeRegistry::getPointerType(baseType.value());
        } else if (auto arrayType = dynamic_cast<ast::ArrayRawType *>(rawType)) {
            auto baseType = resolveFromRawType(arrayType->baseType.get(), registry);
            if (!baseType) return std::nullopt;
            return types::TypeRegistry::getArrayType(baseType.value(), arrayType->size);
        } else {
            return registry.getTypeByName(rawType->typeToken.lexical());
        }
    }

    void type_check(ast::FunctionDefinition *node, Context &context);

    void type_check(ast::FunctionCallNode *node, Context &context);

    void type_check(ast::VariableAssignment *node, Context &context);

    void type_check(ast::VariableAccess *node, Context &context);

    void type_check(ast::VariableDeclaration *node, Context &context);

    void type_check(ast::ReturnStatement *node, Context &context);

    void type_check(ast::BinaryExpression *node, Context &context);

    void type_check(ast::IfCondition *node, Context &context);

    void type_check(ast::Comparisson *node, Context &context);

    void type_check(ast::WhileLoop *node, Context &context);

    void type_check(ast::ForLoop *node, Context &context);

    void type_check(ast::ArrayInitializer *node, Context &context);

    void type_check(ast::ArrayAccess *node, Context &context);

    void type_check(ast::ArrayAssignment *node, Context &context);

    void type_check(ast::LogicalExpression *node, Context &context);

    void type_check(ast::TypeCast *node, Context &context);

    void type_check(ast::StructInitialization *node, Context &context);

    void type_check(ast::FieldAccess *node, Context &context);

    void type_check(ast::FieldAssignment *node, Context &context);

    void type_check(ast::BreakStatement *node, Context &context) {
    }

    void type_check(ast::StringConstant *node, Context &context) {
        const auto u8Type = context.registry.getTypeByName("u8").value();

        node->setExpressionType(
            context.registry.getArrayType(u8Type, node->expressionToken().lexical().size() + 1).value());
    }

    void type_check(ast::NumberConstant *node, Context &context) {
        switch (node->numberType()) {
            case ast::NumberType::INTEGER:
                node->setExpressionType(context.registry.getTypeByName("i32").value());
                break;
            case ast::NumberType::FLOAT:
                node->setExpressionType(context.registry.getTypeByName("float").value());
                break;
            case ast::NumberType::CHAR:
                node->setExpressionType(context.registry.getTypeByName("u8").value());
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
        if (const auto ifCond = dynamic_cast<ast::IfCondition *>(node)) {
            return type_check(ifCond, context);
        }
        if (const auto comp = dynamic_cast<ast::Comparisson *>(node)) {
            return type_check(comp, context);
        }
        if (const auto whileLoop = dynamic_cast<ast::WhileLoop *>(node)) {
            return type_check(whileLoop, context);
        }
        if (const auto forLoop = dynamic_cast<ast::ForLoop *>(node)) {
            return type_check(forLoop, context);
        }
        if (const auto breakStmt = dynamic_cast<ast::BreakStatement *>(node)) {
            return type_check(breakStmt, context);
        }
        if (const auto arrayInit = dynamic_cast<ast::ArrayInitializer *>(node)) {
            return type_check(arrayInit, context);
        }
        if (const auto arrayAccess = dynamic_cast<ast::ArrayAccess *>(node)) {
            return type_check(arrayAccess, context);
        }
        if (const auto arrayAssign = dynamic_cast<ast::ArrayAssignment *>(node)) {
            return type_check(arrayAssign, context);
        }
        if (const auto logExpr = dynamic_cast<ast::LogicalExpression *>(node)) {
            return type_check(logExpr, context);
        }
        if (const auto typeCast = dynamic_cast<ast::TypeCast *>(node)) {
            return type_check(typeCast, context);
        }
        if (const auto structDecl = dynamic_cast<ast::StructDeclaration *>(node)) {
            return;
        }
        if (const auto structInit = dynamic_cast<ast::StructInitialization *>(node)) {
            return type_check(structInit, context);
        }
        if (const auto fieldAccess = dynamic_cast<ast::FieldAccess *>(node)) {
            return type_check(fieldAccess, context);
        }
        if (const auto fieldAssign = dynamic_cast<ast::FieldAssignment *>(node)) {
            return type_check(fieldAssign, context);
        }

        context.messages.push_back({
            parser::OutputType::ERROR,
            node->expressionToken(),
            "Unknown AST node that can not be type checked yet."
        });
    }

    void type_check(ast::FieldAssignment *node, Context &context) {
        type_check_base(node->expression(), context);
        const auto it = context.currentVariables.find(node->expressionToken().lexical());
        if (it == context.currentVariables.end()) {
            context.messages.push_back({
                parser::OutputType::ERROR,
                node->expressionToken(),
                "Variable '" + node->expressionToken().lexical() + "' is not declared in this scope."
            });
            return;
        }
        if (auto structType = std::dynamic_pointer_cast<types::StructType>(it->second->type)) {
            const auto field = structType->field(node->fieldName().lexical());
            if (!field) {
                context.messages.push_back({
                    parser::OutputType::ERROR,
                    node->fieldName(),
                    "The Struct '" + node->expressionToken().lexical() + "' does not contain a field with the name'" +
                    node->fieldName().lexical() + "'."
                });
                return;
            }
            if (node->expression()->expressionType() && field->type->name() != node->expression()->expressionType().
                value()->name()) {
                context.messages.push_back({
                    parser::OutputType::ERROR,
                    node->fieldName(),
                    "The Type of the field '" +
                    node->fieldName().lexical() + "' is not equal to the type that wants to be assigned."
                });
            }
            node->setStructType(structType);
            node->setExpressionType(field->type);
        } else {
            context.messages.push_back({
                parser::OutputType::ERROR,
                node->fieldName(),
                "The Struct '" + node->expressionToken().lexical() + "' is not a valid struct type'" +
                node->fieldName().lexical() + "'."
            });
        }
    }

    void type_check(ast::FieldAccess *node, Context &context) {
        const auto it = context.currentVariables.find(node->expressionToken().lexical());
        if (it == context.currentVariables.end()) {
            context.messages.push_back({
                parser::OutputType::ERROR,
                node->expressionToken(),
                "Variable '" + node->expressionToken().lexical() + "' is not declared in this scope."
            });
            return;
        }
        if (auto structType = std::dynamic_pointer_cast<types::StructType>(it->second->type)) {
            auto field = structType->field(node->fieldName().lexical());
            if (!field) {
                context.messages.push_back({
                    parser::OutputType::ERROR,
                    node->fieldName(),
                    "The Struct '" + node->expressionToken().lexical() + "' does not contain a field with the name'" +
                    node->fieldName().lexical() + "'."
                });
                return;
            }
            node->setStructType(structType);
            node->setExpressionType(field->type);
        } else {
            context.messages.push_back({
                parser::OutputType::ERROR,
                node->fieldName(),
                "The Struct '" + node->expressionToken().lexical() + "' is not a valid struct type'" +
                node->fieldName().lexical() + "'."
            });
        }
    }

    void type_check_field(const ast::StructInitField &field, Context &context) {
        type_check_base(field.value.get(), context);
    }

    void type_check(ast::StructInitialization *node, Context &context) {
        const auto type = context.registry.getTypeByName(node->expressionToken().lexical());
        if (type) {
            if (auto structType = std::dynamic_pointer_cast<types::StructType>(type.value())) {
                for (auto &field: node->fields()) {
                    type_check_field(field, context);
                }
                node->setExpressionType(type.value());
            } else {
                context.messages.push_back({
                    .outputType = parser::OutputType::ERROR,
                    .token = node->expressionToken(),
                    .message = "The type " + node->expressionToken().lexical() +
                               " is not a valid structure type."
                });
            }
        } else {
            context.messages.push_back({
                .outputType = parser::OutputType::ERROR,
                .token = node->expressionToken(),
                .message = "Could not determine type " + node->expressionToken().lexical() +
                           " for the structure initialization."
            });
        }
    }

    void type_check(ast::TypeCast *node, Context &context) {
        auto type = resolveFromRawType(node->rawType(), context.registry);
        if (!type) {
            context.messages.push_back({
                parser::OutputType::ERROR,
                node->rawType()->typeToken,
                "Unknown type '" + node->rawType()->typeToken.lexical() + "' in type cast."
            });
            return;
        }
        node->setExpressionType(type.value());
        type_check_base(node->value(), context);
        if (!node->value()->expressionType()) {
            context.messages.push_back({
                parser::OutputType::ERROR,
                node->value()->expressionToken(),
                "Could not determine type of expression in type cast."
            });
            return;
        }
    }

    void type_check(ast::LogicalExpression *node, Context &context) {
        type_check_base(node->lhs(), context);
        type_check_base(node->rhs(), context);
        if (node->lhs()->expressionType() && node->rhs()->expressionType()) {
            if (node->lhs()->expressionType().value()->name() != "bool" ||
                node->rhs()->expressionType().value()->name() != "bool") {
                context.messages.push_back({
                    parser::OutputType::ERROR,
                    node->expressionToken(),
                    "Logical expressions require boolean operands, but got '" +
                    node->lhs()->expressionType().value()->name() + "' and '" +
                    node->rhs()->expressionType().value()->name() + "'."
                });
                return;
            }
            node->setExpressionType(context.registry.getTypeByName("bool").value());
        } else {
            context.messages.push_back({
                parser::OutputType::ERROR,
                node->expressionToken(),
                "Could not determine types of operands in logical expression."
            });
        }
    }


    bool type_check_range_for(ast::ForLoop *node, Context &context) {
        type_check_base(node->rangeEnd(), context);
        if (node->rangeStart()->expressionType() && node->rangeEnd()->expressionType()) {
            if (node->rangeStart()->expressionType().value()->name() != node->rangeEnd()->expressionType().value()->
                name()) {
                context.messages.push_back({
                    parser::OutputType::ERROR,
                    node->expressionToken(),
                    "Type mismatch in 'for' loop range: start is of type '" +
                    node->rangeStart()->expressionType().value()->name() + "', end is of type '" +
                    node->rangeEnd()->expressionType().value()->name() + "'."
                });
                return true;
            }
            if (node->rangeStart()->expressionType().value()->name() != "i32" &&
                node->rangeStart()->expressionType().value()->name() != "i64") {
                context.messages.push_back({
                    parser::OutputType::ERROR,
                    node->expressionToken(),
                    "'for' loop range must be of integer type, but got '" +
                    node->rangeStart()->expressionType().value()->name() + "'."
                });
                return true;
            }
            node->setExpressionType(node->rangeStart()->expressionType().value());
        } else {
            context.messages.push_back({
                parser::OutputType::ERROR,
                node->expressionToken(),
                "Could not determine types of 'for' loop range."
            });
            return true;
        }
        return false;
    }

    bool type_check_iterator_loop(ast::ForLoop *node, Context &context) {
        auto varType = node->rangeStart()->expressionType();
        if (!varType) {
            return true;
        }
        if (auto arrayType = dynamic_cast<ArrayType *>(varType.value().get())) {
            node->setExpressionType(arrayType->baseType());
        }

        return false;
    }

    void type_check(ast::ForLoop *node, Context &context) {
        type_check_base(node->rangeStart(), context);
        if (node->rangeEnd()) {
            if (type_check_range_for(node, context)) return;
        } else {
            if (type_check_iterator_loop(node, context)) return;
        }
        // Create a new scope for the loop variable
        const auto varType = node->rangeStart()->expressionType().value();

        context.currentVariables.emplace(node->iteratorToken().lexical(),
                                         Variable{
                                             node->iteratorToken().lexical(), varType,
                                             false
                                         });
        for (auto &stmt: node->block()) {
            type_check_base(stmt.get(), context);
        }
    }

    void type_check(ast::WhileLoop *node, Context &context) {
        type_check_base(node->condition(), context);
        for (auto &stmt: node->block()) {
            type_check_base(stmt.get(), context);
        }
    }

    void type_check(ast::IfCondition *node, Context &context) {
        type_check_base(node->condition(), context);
        for (auto &stmt: node->ifBlock()) {
            type_check_base(stmt.get(), context);
        }
        for (auto &stmt: node->elseBlock()) {
            type_check_base(stmt.get(), context);
        }
    }

    void type_check(ast::Comparisson *node, Context &context) {
        const auto lhs = node->lhs();
        const auto rhs = node->rhs();
        type_check_base(lhs, context);
        type_check_base(rhs, context);
        if (lhs->expressionType() && rhs->expressionType()) {
            if (lhs->expressionType().value()->name() != rhs->expressionType().value()->name()) {
                context.messages.push_back({
                    parser::OutputType::ERROR,
                    node->expressionToken(),
                    "Type mismatch in comparison: left is of type '" +
                    lhs->expressionType().value()->name() + "', right is of type '" +
                    rhs->expressionType().value()->name() + "'."
                });
                return;
            }
            node->setExpressionType(context.registry.getTypeByName("bool").value());
        } else {
            context.messages.push_back({
                parser::OutputType::ERROR,
                node->expressionToken(),
                "Could not determine types of operands in comparison."
            });
        }
    }

    void type_check(ast::ArrayInitializer *node, Context &context) {
        auto elementType = std::shared_ptr<VariableType>(nullptr);
        for (auto &element: node->elements()) {
            if (elementType == nullptr) {
                type_check_base(element.get(), context);
                if (element->expressionType()) {
                    elementType = element->expressionType().value();
                } else {
                    context.messages.push_back({
                        parser::OutputType::ERROR,
                        node->expressionToken(),
                        "Could not determine type of array element."
                    });
                    return;
                }
            } else {
                type_check_base(element.get(), context);
                if (element->expressionType()) {
                    if (element->expressionType().value()->name() != elementType->name()) {
                        context.messages.push_back({
                            parser::OutputType::ERROR,
                            node->expressionToken(),
                            "Type mismatch in array initializer: expected element of type '" +
                            elementType->name() + "', but got '" + element->expressionType().value()->name() + "'."
                        });
                        return;
                    }
                    node->setExpressionType(
                        context.registry.getArrayType(element->expressionType().value(), node->elements().size()).
                        value());
                } else {
                    context.messages.push_back({
                        parser::OutputType::ERROR,
                        node->expressionToken(),
                        "Could not determine type of array element."
                    });
                    return;
                }
            }
        }
        if (node->elements().empty()) {
            context.messages.push_back({
                parser::OutputType::ERROR,
                node->expressionToken(),
                "Array initializer cannot be empty."
            });
            return;
        }
    }

    void type_check(ast::ArrayAssignment *node, Context &context) {
        const auto it = context.currentVariables.find(node->arrayToken().lexical());
        if (it == context.currentVariables.end()) {
            context.messages.push_back({
                parser::OutputType::ERROR,
                node->arrayToken(),
                "Variable '" + node->arrayToken().lexical() + "' is not declared in this scope."
            });
            return;
        }
        if (it->second.has_value()) {
            if (it->second->type->typeKind() != TypeKind::ARRAY && it->second->type->typeKind() !=
                TypeKind::POINTER) {
                context.messages.push_back({
                    parser::OutputType::ERROR,
                    node->arrayToken(),
                    "Variable '" + node->arrayToken().lexical() + "' is not an array."
                });
                return;
            }
            type_check_base(node->index(), context);
            if (!node->index()->expressionType() || node->index()->expressionType().value()->name() != "i32") {
                context.messages.push_back({
                    parser::OutputType::ERROR,
                    node->index()->expressionToken(),
                    "Array index must be of type 'i32'."
                });
                return;
            }
            type_check_base(node->value(), context);
            if (!node->value()->expressionType()) {
                context.messages.push_back({
                    parser::OutputType::ERROR,
                    node->value()->expressionToken(),
                    "Could not determine type of value being assigned to array."
                });
                return;
            }
            if (auto arrayType = std::dynamic_pointer_cast<ArrayType>(it->second->type)) {
                if (arrayType->baseType()->name() != node->value()->expressionType().value()->name()) {
                    context.messages.push_back({
                        parser::OutputType::ERROR,
                        node->value()->expressionToken(),
                        "Type mismatch in array assignment: array '" + node->arrayToken().lexical() +
                        "' expects elements of type '" + arrayType->baseType()->name() +
                        "', but got value of type '" + node->value()->expressionType().value()->name() + "'."
                    });
                    return;
                }
            } else if (auto pointerType = std::dynamic_pointer_cast<PointerType>(it->second->type)) {
                if (pointerType->baseType()->name() != node->value()->expressionType().value()->name()) {
                    context.messages.push_back({
                        parser::OutputType::ERROR,
                        node->value()->expressionToken(),
                        "Type mismatch in array assignment: pointer '" + node->arrayToken().lexical() +
                        "' expects elements of type '" + pointerType->baseType()->name() +
                        "', but got value of type '" + node->value()->expressionType().value()->name() + "'."
                    });
                    return;
                }
            } else {
                context.messages.push_back({
                    parser::OutputType::ERROR,
                    node->arrayToken(),
                    "Internal error: variable '" + node->arrayToken().lexical() +
                    "' type information is corrupted."
                });
            }
            node->setArrayType(it->second->type);
        } else {
            context.messages.push_back({
                parser::OutputType::ERROR,
                node->arrayToken(),
                "Variable '" + node->arrayToken().lexical() + "' has no type information."
            });
        }
    }

    void type_check(ast::ArrayAccess *node, Context &context) {
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
            if (it->second->type->typeKind() != TypeKind::ARRAY) {
                context.messages.push_back({
                    parser::OutputType::ERROR,
                    node->expressionToken(),
                    "Variable '" + node->expressionToken().lexical() + "' is not an array."
                });
                return;
            }
            type_check_base(node->index(), context);
            if (!node->index()->expressionType() || node->index()->expressionType().value()->name() != "i32") {
                context.messages.push_back({
                    parser::OutputType::ERROR,
                    node->index()->expressionToken(),
                    "Array index must be of type 'i32'."
                });
                return;
            }
            if (auto arrayType = std::dynamic_pointer_cast<ArrayType>(it->second->type)) {
                node->setExpressionType(arrayType->baseType());
                node->setArrayType(arrayType);
            } else {
                context.messages.push_back({
                    parser::OutputType::ERROR,
                    node->expressionToken(),
                    "Internal error: variable '" + node->expressionToken().lexical() +
                    "' type information is corrupted."
                });
            }
        } else {
            context.messages.push_back({
                parser::OutputType::ERROR,
                node->expressionToken(),
                "Variable '" + node->expressionToken().lexical() + "' has no type information."
            });
        }
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
        //  find function
        if (node->functionName() == "println") {
            if (node->args().size() != 1) {
                context.messages.push_back({
                    parser::OutputType::ERROR,
                    node->expressionToken(),
                    "'println' expects exactly one argument."
                });
                return;
            }
            // 'println' can accept any type for now
            node->setExpressionType(context.registry.getTypeByName("void").value());
            return;
        }
        for (const auto funcDef: context.functions) {
            if (funcDef->functionName() == node->functionName()) {
                if (funcDef->args().size() != node->args().size()) {
                    context.messages.push_back({
                        parser::OutputType::ERROR,
                        node->expressionToken(),
                        "Function '" + node->functionName() + "' expects " +
                        std::to_string(funcDef->args().size()) + " arguments, but got " +
                        std::to_string(node->args().size()) + "."
                    });
                    return;
                }
                bool argsMatch = true;
                for (size_t i = 0; i < funcDef->args().size(); ++i) {
                    if (!node->args()[i]->expressionType() ||
                        funcDef->args()[i].type == nullptr ||
                        node->args()[i]->expressionType().value()->name() !=
                        funcDef->args()[i].type.value()->name()) {
                        context.messages.push_back({
                            parser::OutputType::ERROR,
                            node->args()[i]->expressionToken(),
                            "Type mismatch for argument " + std::to_string(i + 1) + " in function '" +
                            node->functionName() + "': expected '" +
                            (funcDef->args()[i].type ? funcDef->args()[i].type.value()->name() : "unknown") +
                            "', but got '" +
                            (node->args()[i]->expressionType()
                                 ? node->args()[i]->expressionType().value()->name()
                                 : "unknown") + "'."
                        });
                        argsMatch = false;
                    }
                }
                if (!argsMatch) return;
                if (funcDef->returnType()) {
                    node->setExpressionType(
                        resolveFromRawType(funcDef->returnType().value(), context.registry).value());
                } else {
                    node->setExpressionType(context.registry.getTypeByName("void").value());
                }
                return;
            }
        }
        context.messages.push_back({
            parser::OutputType::ERROR,
            node->expressionToken(),
            "Function '" + node->functionSignature() + "' is not declared."
        });

        // Further type checking logic would go here...
    }

    void type_check(ast::ReturnStatement *node, Context &context) {
        if (node->returnValue()) {
            type_check_base(node->returnValue().value(), context);
        }
        // Further type checking logic would go here...
    }


    void type_check(ast::VariableDeclaration *node, Context &context) {
        auto varType = resolveFromRawType(node->type(), context.registry);
        if (!varType) {
            context.messages.push_back({
                parser::OutputType::ERROR,
                node->expressionToken(),
                "Unknown type '" + node->type()->typeToken.lexical() + "' for variable '" + node->expressionToken().
                lexical() +
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
            const auto initalType = node->initialValue().value()->expressionType();
            const auto declaredType = node->expressionType();
            if (!initalType) {
                context.messages.push_back({
                    parser::OutputType::ERROR,
                    node->initialValue().value()->expressionToken(),
                    "Could not determine type of initial value for variable '" + node->expressionToken().lexical() +
                    "'."
                });
            }
            if (initalType && declaredType && initalType.value()->name() != declaredType.value()->name()) {
                context.messages.push_back({
                    parser::OutputType::ERROR,
                    node->initialValue().value()->expressionToken(),
                    "Type mismatch in variable initialization: variable '" + node->expressionToken().lexical() +
                    "' is of type '" + (declaredType ? declaredType.value()->name() : "unknown") +
                    "', but got initial value of type '" + (initalType ? initalType.value()->name() : "unknown") +
                    "'."
                });
            }
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
            arg.type = resolveFromRawType(arg.rawType.get(), context.registry);
            if (!arg.type) {
                context.messages.push_back({
                    parser::OutputType::ERROR,
                    node->expressionToken(),
                    "Unknown type '" + arg.rawType->typeToken.lexical() + "' for argument '" + arg.name + "'."
                });
            }
            context.currentVariables.emplace(arg.name, Variable{arg.name, arg.type.value_or(nullptr), false});
        }
        for (auto &stmt: node->statements()) {
            type_check_base(stmt.get(), context);
        }
        context.currentVariables.clear();

        if (node->returnType()) {
            if (const auto returnType = resolveFromRawType(node->returnType().value(), context.registry))
                node->setExpressionType(returnType.value());
            else {
                context.messages.push_back({
                    parser::OutputType::ERROR,
                    node->expressionToken(),
                    "Unknown type '" + node->returnType().value()->typeToken.lexical() + "' for the function retu."
                });
            }
        } else {
            node->setExpressionType(context.registry.getTypeByName("void").value());
        }
        // Further type checking logic would go here...
    }

    void type_check(const std::vector<std::unique_ptr<ast::ASTNode> > &nodes, TypeCheckResult &result) {
        Context context;
        // resolve global type declarations first
        for (const auto &node: nodes) {
            if (auto structDecl = dynamic_cast<ast::StructDeclaration *>(node.get())) {
                std::vector<types::StructField> structFields;
                for (auto &field: structDecl->fields()) {
                    structFields.push_back({
                        .type = resolveFromRawType(field.type.get(), context.registry).value(),
                        .name = field.name.lexical()
                    });
                }
                auto type = std::make_shared<types::StructType>(structDecl->expressionToken().lexical(),
                                                                structFields);
                context.registry.registerType(type);
            }
        }
        // collect function definitions second
        for (const auto &node: nodes) {
            if (const auto funcDef = dynamic_cast<ast::FunctionDefinition *>(node.get())) {
                context.functions.push_back(funcDef);
            }
        }

        for (auto &node: nodes) {
            type_check_base(node.get(), context);
        }
        result.messages = std::move(context.messages);
    }
}
