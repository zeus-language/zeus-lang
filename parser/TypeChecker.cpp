#include "types/TypeChecker.h"

#include <cassert>
#include <cmath>
#include <map>
#include <ranges>

#include "ast/ArrayAccess.h"
#include "ast/ArrayAssignment.h"
#include "ast/ArrayInitializer.h"
#include "ast/BinaryExpression.h"
#include "ast/BreakStatement.h"
#include "ast/Comparisson.h"
#include "ast/EnumAccess.h"
#include "ast/EnumDeclaration.h"
#include "ast/ExternFunctionDefinition.h"
#include "ast/FieldAccess.h"
#include "ast/FieldAssignment.h"
#include "ast/ForLoop.h"
#include "ast/FunctionCallNode.h"
#include "ast/FunctionDefinition.h"
#include "ast/IfCondition.h"
#include "ast/LogicalExpression.h"
#include "ast/MatchExpression.h"
#include "ast/MethodCallNode.h"
#include "ast/NumberConstant.h"
#include "ast/RangeExpression.h"
#include "ast/ReferenceAccess.h"
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
        std::shared_ptr<parser::Module> module;

        [[nodiscard]] std::vector<ast::FunctionDefinitionBase *> findFunctionsByName(const std::string &path,
            const std::string &name) const {
            std::vector<ast::FunctionDefinitionBase *> result;
            const std::optional<std::string> aliasName = module->aliasName.has_value()
                                                             ? std::make_optional(module->aliasName.value() + "::")
                                                             : std::nullopt;

            for (const auto &f: module->functions) {
                if (f->functionName() == name and (
                        f->modulePathName() == path or f->modulePathName() == module->modulePathName()
                        or (aliasName and path == aliasName.value())
                        or (path.empty() and !aliasName)
                    )) {
                    result.push_back(f.get());
                }
            }

            for (const auto &m: module->modules) {
                std::optional<std::string> aliasName2 = m->aliasName.has_value()
                                                            ? std::make_optional(m->aliasName.value() + "::")
                                                            : std::nullopt;
                if (m->modulePathName() == path or (aliasName2 and path == aliasName2.value())
                    or (path.empty() and !aliasName2)
                ) {
                    for (const auto &node: m->functions) {
                        if (node->functionName() == name) {
                            result.push_back(node.get());
                        }
                    }
                }
            }

            return result;
        }
    };


    std::optional<std::shared_ptr<VariableType> > resolveFromRawType(ast::RawType *rawType, TypeRegistry &registry) {
        if (const auto arrayType = dynamic_cast<ast::ArrayRawType *>(rawType)) {
            const auto baseType = resolveFromRawType(arrayType->baseType.get(), registry);
            if (!baseType) return std::nullopt;
            auto type = types::TypeRegistry::getArrayType(baseType.value(), arrayType->size);
            if (rawType->typeModifier == ast::TypeModifier::POINTER) {
                if (!type) return std::nullopt;
                return types::TypeRegistry::getPointerType(type.value());
            } else if (rawType->typeModifier == ast::TypeModifier::REFERENCE) {
                if (!type) return std::nullopt;
                return types::TypeRegistry::getReferenceType(type.value());
            }
            return type;
        } else if (rawType->typeModifier == ast::TypeModifier::POINTER) {
            const auto baseType = registry.getTypeByName(rawType->typeToken.lexical());
            if (!baseType) return std::nullopt;
            return types::TypeRegistry::getPointerType(baseType.value());
        } else if (rawType->typeModifier == ast::TypeModifier::REFERENCE) {
            const auto baseType = registry.getTypeByName(rawType->typeToken.lexical());
            if (!baseType) return std::nullopt;
            return types::TypeRegistry::getReferenceType(baseType.value());
        } else {
            return registry.getTypeByName(rawType->typeToken.lexical());
        }
    }

    void type_check(ast::FunctionDefinition *node, Context &context);

    void type_check(ast::ExternFunctionDefinition *node, Context &context);

    void type_check(ast::FunctionCallNode *node, Context &context);

    void type_check(const ast::VariableAssignment *node, Context &context);

    void type_check(ast::VariableAccess *node, Context &context);

    void type_check(ast::VariableDeclaration *node, Context &context);

    void type_check(const ast::ReturnStatement *node, Context &context);

    void type_check(ast::BinaryExpression *node, Context &context);

    void type_check(ast::IfCondition *node, Context &context);

    void type_check(ast::Comparisson *node, Context &context);

    void type_check(const ast::WhileLoop *node, Context &context);

    void type_check(ast::ForLoop *node, Context &context);

    void type_check(ast::ArrayInitializer *node, Context &context);

    void type_check(ast::ArrayAccess *node, Context &context);

    void type_check(ast::ArrayAssignment *node, Context &context);

    void type_check(ast::LogicalExpression *node, Context &context);

    void type_check(ast::TypeCast *node, Context &context);

    void type_check(ast::StructInitialization *node, Context &context);

    void type_check(ast::FieldAccess *node, Context &context);

    void type_check(ast::FieldAssignment *node, Context &context);

    void type_check(ast::ReferenceAccess *node, Context &context);

    void type_check(ast::MatchExpression *node, Context &context);

    void type_check(ast::RangeExpression *node, Context &context);

    void type_check(ast::EnumAccess *node, Context &context);

    void type_check(ast::MethodCallNode *node, Context &context);

    void type_check(ast::BreakStatement *node, Context &context) {
    }

    void type_check(ast::StringConstant *node, const Context &context) {
        const auto u8Type = context.registry.getTypeByName("u8").value();

        node->setExpressionType(
            types::TypeRegistry::getArrayType(u8Type, node->expressionToken().lexical().size() + 1).value());
    }

    void type_check(ast::NumberConstant *node, const Context &context) {
        switch (node->numberType()) {
            case ast::NumberType::INTEGER: {
                if (node->numBits() == 32) {
                    node->setExpressionType(context.registry.getTypeByName("i32").value());
                } else {
                    node->setExpressionType(context.registry.getTypeByName("i64").value());
                }
            }
            break;
            case ast::NumberType::FLOAT:
                node->setExpressionType(context.registry.getTypeByName("float").value());
                break;
            case ast::NumberType::CHAR:
                node->setExpressionType(context.registry.getTypeByName("u8").value());
                break;
            case ast::NumberType::BOOLEAN:
                node->setExpressionType(context.registry.getTypeByName("bool").value());
                break;
            default:
                assert(false && "Unknown number type");
        }
    }


    void type_check_base(ast::ASTNode *node, Context &context) {
        if (const auto funcDef = dynamic_cast<ast::FunctionDefinition *>(node)) {
            return type_check(funcDef, context);
        }
        if (const auto funcDef = dynamic_cast<ast::ExternFunctionDefinition *>(node)) {
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
        if (dynamic_cast<ast::StructDeclaration *>(node)) {
            return;
        }
        if (dynamic_cast<ast::EnumDeclaration *>(node)) {
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
        if (const auto refAccess = dynamic_cast<ast::ReferenceAccess *>(node)) {
            return type_check(refAccess, context);
        }

        if (const auto matchExpr = dynamic_cast<ast::MatchExpression *>(node)) {
            return type_check(matchExpr, context);
        }
        if (const auto rangeExpr = dynamic_cast<ast::RangeExpression *>(node)) {
            return type_check(rangeExpr, context);
        }
        if (const auto enumAccess = dynamic_cast<ast::EnumAccess *>(node)) {
            return type_check(enumAccess, context);
        }
        if (const auto methodCall = dynamic_cast<ast::MethodCallNode *>(node)) {
            return type_check(methodCall, context);
        }
        assert(node != nullptr && "Node is null");

        context.messages.push_back({
            parser::OutputType::ERROR,
            node->expressionToken(),
            "Unknown AST node that can not be type checked yet."
        });
    }

    void type_check(ast::MethodCallNode *node, Context &context) {
        type_check_base(node->instanceNode(), context);
        for (auto &arg: node->args()) {
            type_check_base(arg.get(), context);
        }

        if (!node->instanceNode()->expressionType()) {
            context.messages.push_back({
                parser::OutputType::ERROR,
                node->expressionToken(),
                "Could not determine type of method call instance."
            });
            return;
        }

        const auto methodName = node->functionName();
        const ast::FunctionDefinitionBase *matchedMethod = nullptr;

        auto instanceType = node->instanceNode()->expressionType().value();
        if (const auto refAccess = dynamic_cast<ast::ReferenceAccess *>(node->instanceNode())) {
            if (refAccess->expressionType()) {
                if (const auto ptrType = std::dynamic_pointer_cast<
                    types::ReferenceType>(refAccess->expressionType().value())) {
                    instanceType = ptrType->baseType();
                }
            }
        }


        if (const auto structType = std::dynamic_pointer_cast<types::StructType>(instanceType)) {
            const auto methods = structType->methods();
            auto filteredMethods = methods | std::ranges::views::filter([methodName](auto method) {
                return method->functionName() == methodName;
            });
            if (filteredMethods.empty()) {
                context.messages.push_back({
                    parser::OutputType::ERROR,
                    node->expressionToken(),
                    "Type '" + instanceType->name() + "' does not have a method named '" + methodName + "'."
                });
                return;
            }


            for (const auto method: filteredMethods) {
                if (method->args().size() - 1 != node->args().size()) {
                    continue;
                }
                const auto oldVars = context.currentVariables;
                type_check_base(method, context);
                context.currentVariables = oldVars;

                bool allParamsMatch = true;
                for (size_t i = 1; i < method->args().size(); ++i) {
                    if (node->args()[i - 1]->expressionType() == nullptr ||
                        node->args()[i - 1]->expressionType().value()->name() !=
                        method->args()[i].type.value()->name()) {
                        allParamsMatch = false;
                        break;
                    }
                }

                if (allParamsMatch) {
                    matchedMethod = method;
                    break;
                }
            }
        } else if (auto arrayType = std::dynamic_pointer_cast<types::ArrayType>(instanceType)) {
            // OK
            if (methodName == "length" && node->args().empty()) {
                node->setExpressionType(context.registry.getTypeByName("i32").value());
                return;
            } else {
                context.messages.push_back({
                    parser::OutputType::ERROR,
                    node->expressionToken(),
                    "Array type does not have a method named '" + methodName + "'."
                });
                return;
            }
        } else if (auto enumType = std::dynamic_pointer_cast<types::EnumType>(instanceType)) {
            // OK
        } else {
            context.messages.push_back({
                parser::OutputType::ERROR,
                node->expressionToken(),
                "Type '" + instanceType->name() + "' does not support method calls."
            });
            return;
        }


        if (!matchedMethod) {
            context.messages.push_back({
                parser::OutputType::ERROR,
                node->expressionToken(),
                "No matching overload found for method '" + methodName + "' with the given argument types."
            });
            return;
        }
        if (matchedMethod->expressionType())
            node->setExpressionType(matchedMethod->expressionType().value());
    }

    void type_check(ast::EnumAccess *node, Context &context) {
        if (const auto type = context.registry.getTypeByName(node->expressionToken().lexical())) {
            if (const auto enumType = std::dynamic_pointer_cast<types::EnumType>(
                type.value())) {
                const auto variantIt = std::ranges::find_if(enumType->variants(),
                                                            [&](const types::EnumVariant &variant) {
                                                                return variant.name == node->variantName().lexical();
                                                            });
                if (variantIt == enumType->variants().end()) {
                    context.messages.push_back({
                        parser::OutputType::ERROR,
                        node->expressionToken(),
                        "Enum '" + enumType->name() + "' does not have a variant named '" +
                        node->variantName().lexical() + "'."
                    });
                    return;
                }
                node->setExpressionType(type.value());
            } else {
                context.messages.push_back({
                    parser::OutputType::ERROR,
                    node->expressionToken(),
                    "Attempting to access enum variant on a non-enum type '" +
                    node->variantName().lexical() + "'."
                });
                return;
            }
        } else {
            context.messages.push_back({
                parser::OutputType::ERROR,
                node->expressionToken(),
                "Could not determine type of enum access base."
            });
        }
    }

    void type_check(ast::RangeExpression *node, Context &context) {
        type_check_base(node->start(), context);
        type_check_base(node->end(), context);
        if (node->start()->expressionType() && node->end()->expressionType()) {
            if (node->start()->expressionType().value()->name() !=
                node->end()->expressionType().value()->name()) {
                context.messages.push_back({
                    parser::OutputType::ERROR,
                    node->expressionToken(),
                    "Type mismatch in range expression: start is of type '" +
                    node->start()->expressionType().value()->name() +
                    "', but end is of type '" + node->end()->expressionType().value()->name() + "'."
                });
                return;
            }

            node->setExpressionType(node->start()->expressionType().value());
        } else {
            context.messages.push_back({
                parser::OutputType::ERROR,
                node->expressionToken(),
                "Could not determine types in range expression."
            });
            return;
        }
    }

    void type_check(ast::MatchExpression *node, Context &context) {
        type_check_base(node->accessNode(), context);
        for (auto &[matchKeys, expression]: node->matchCases()) {
            for (auto &key: matchKeys) {
                type_check_base(key.get(), context);
                if (key->expressionType() != node->accessNode()->expressionType() and
                    key->expressionToken().lexical() != "_"
                ) {
                    context.messages.push_back({
                        parser::OutputType::ERROR,
                        key->expressionToken(),
                        "The case element has a diffent type than the match expression!"
                    });
                }
            }
            type_check_base(expression.get(), context);
            if (node->accessNode()->expressionType()) {
                node->setExpressionType(node->accessNode()->expressionType().value());
            }
        }
    }

    void type_check(ast::ReferenceAccess *node, Context &context) {
        type_check_base(node->accessNode(), context);
        if (node->accessNode()->expressionType()) {
            node->setExpressionType(
                types::TypeRegistry::getReferenceType(node->accessNode()->expressionType().value()).value());
        } else {
            context.messages.push_back({
                parser::OutputType::ERROR,
                node->expressionToken(),
                "Could not determine type of reference access base."
            });
        }
    }

    void type_check(ast::FieldAssignment *node, Context &context) {
        type_check_base(node->expression(), context);
        type_check_base(node->accessNode(), context);
        if (node->accessNode()->expressionType() && node->expression()->expressionType()) {
            if (node->accessNode()->expressionType().value()->name() != node->expression()->expressionType().value()->
                name()) {
                context.messages.push_back({
                    parser::OutputType::ERROR,
                    node->expressionToken(),
                    "Type mismatch in field assignment: field '" + node->accessNode()->expressionToken().
                    lexical() +
                    "' is of type '" + node->accessNode()->expressionType().value()->name() +
                    "', but assigned expression is of type '" + node->expression()->expressionType().value()->name() +
                    "'."
                });
                return;
            }
            if (const auto accessNode = dynamic_cast<ast::FieldAccess *>(node->accessNode())) {
                node->setStructType(accessNode->structType().value());
            }

            node->setExpressionType(node->expression()->expressionType().value());
        } else {
            context.messages.push_back({
                parser::OutputType::ERROR,
                node->expressionToken(),
                "Could not determine types in field assignment."
            });
            return;
        }

        if (node->accessNode()->constant()) {
            context.messages.push_back({
                parser::OutputType::ERROR,
                node->expressionToken(),
                "cannot modify a field of an immutable variable."
            });
            return;
        }
    }

    void type_check(ast::FieldAccess *node, Context &context) {
        type_check_base(node->accessNode(), context);
        if (node->accessNode()) {
            if (const auto varAccess = dynamic_cast<ast::VariableAccess *>(node->accessNode())) {
                if (!varAccess->expressionType()) {
                    context.messages.push_back({
                        parser::OutputType::ERROR,
                        node->expressionToken(),
                        "Could not determine type of variable '" + varAccess->expressionToken().lexical() +
                        "' in field access."
                    });
                    return;
                }
                std::shared_ptr<types::StructType> structType = nullptr;
                if (const auto referenceType = std::dynamic_pointer_cast<
                    types::ReferenceType>(varAccess->expressionType().value())) {
                    if (const auto _structType = std::dynamic_pointer_cast<
                        types::StructType>(referenceType->baseType())) {
                        structType = _structType;
                    }
                } else if (const auto _structType = std::dynamic_pointer_cast<
                    types::StructType>(varAccess->expressionType().value())) {
                    structType = _structType;
                }
                if (structType) {
                    const auto fieldIt = std::ranges::find_if(structType->fields(),
                                                              [&](const types::StructField &field) {
                                                                  return field.name == node->fieldName().lexical();
                                                              });
                    if (fieldIt == structType->fields().end()) {
                        context.messages.push_back({
                            parser::OutputType::ERROR,
                            node->expressionToken(),
                            "Field '" + node->fieldName().lexical() + "' does not exist in struct type '" +
                            structType->name() + "'."
                        });
                        return;
                    }
                    node->setExpressionType(fieldIt->type);
                    node->setStructType(varAccess->expressionType().value());
                } else {
                    context.messages.push_back({
                        parser::OutputType::ERROR,
                        node->expressionToken(),
                        "Type '" + varAccess->expressionType().value()->name() +
                        "' is not a struct type, cannot access field '" + node->fieldName().lexical() + "'."
                    });
                    return;
                }
            } else if (const auto fieldAccess = dynamic_cast<ast::FieldAccess *>(node->accessNode())) {
                if (!fieldAccess->expressionType()) {
                    context.messages.push_back({
                        parser::OutputType::ERROR,
                        node->expressionToken(),
                        "Could not determine type of field '" + fieldAccess->fieldName().lexical() +
                        "' in nested field access."
                    });
                    return;
                }
                if (const auto structType = std::dynamic_pointer_cast<types::StructType>(
                    fieldAccess->expressionType().value())) {
                    const auto fieldIt = std::ranges::find_if(structType->fields(),
                                                              [&](const types::StructField &field) {
                                                                  return field.name == node->fieldName().lexical();
                                                              });
                    if (fieldIt == structType->fields().end()) {
                        context.messages.push_back({
                            parser::OutputType::ERROR,
                            node->expressionToken(),
                            "Field '" + node->fieldName().lexical() + "' does not exist in struct type '" +
                            structType->name() + "'."
                        });
                        return;
                    }
                    node->setExpressionType(fieldIt->type);
                    node->setStructType(fieldAccess->expressionType().value());
                }
            } else {
                context.messages.push_back({
                    parser::OutputType::ERROR,
                    node->expressionToken(),
                    "Field access base is not a variable or field access."
                });
                return;
            }
        }
        assert(node->structType() && "Struct type is null in field access");
    }

    void type_check_field(const ast::StructInitField &field, Context &context) {
        type_check_base(field.value.get(), context);
    }

    void type_check(ast::StructInitialization *node, Context &context) {
        if (const auto type = context.registry.getTypeByName(node->expressionToken().lexical())) {
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
        const auto type = resolveFromRawType(node->rawType(), context.registry);
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


    bool type_check_iterator_loop(ast::ForLoop *node, Context &context) {
        const auto varType = node->range()->expressionType();
        if (!varType) {
            context.messages.push_back({
                parser::OutputType::ERROR,
                node->expressionToken(),
                "Could not determine type of the for loop range expression."
            });
            return true;
        }
        if (const auto arrayType = dynamic_cast<ArrayType *>(varType.value().get())) {
            node->setExpressionType(arrayType->baseType());
        } else {
            node->setExpressionType(varType.value());
        }

        return false;
    }

    void type_check(ast::ForLoop *node, Context &context) {
        type_check_base(node->range(), context);
        if (type_check_iterator_loop(node, context)) return;
        // Create a new scope for the loop variable
        const auto varType = node->range()->expressionType().value();

        context.currentVariables.emplace(node->iteratorToken().lexical(),
                                         Variable{
                                             node->iteratorToken().lexical(), varType,
                                             false
                                         });
        for (auto &stmt: node->block()) {
            type_check_base(stmt.get(), context);
        }
    }

    void type_check(const ast::WhileLoop *node, Context &context) {
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
                        types::TypeRegistry::getArrayType(element->expressionType().value(), node->elements().size()).
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
        type_check_base(node->accessNode(), context);
        type_check_base(node->value(), context);
        type_check_base(node->index(), context);
        const auto varType = node->accessNode()->expressionType();
        if (!varType) {
            context.messages.push_back({
                parser::OutputType::ERROR,
                node->expressionToken(),
                "Type of the array access expression could not be determined."
            });
            return;
        }
        node->setArrayType(varType.value());
        if (const auto arrayType = dynamic_cast<ArrayType *>(varType.value().get())) {
            node->setExpressionType(arrayType->baseType());
        } else if (const auto pointerType = dynamic_cast<PointerType *>(varType.value().get())) {
            node->setExpressionType(pointerType->baseType());
        } else {
            context.messages.push_back({
                parser::OutputType::ERROR,
                node->expressionToken(),
                "Type '" + varType.value()->name() + "' is not an array or pointer type."
            });
            return;
        }
        if (node->expressionType() && node->value()->expressionType()) {
            if (node->expressionType().value()->name() != node->value()->expressionType().value()->name()) {
                context.messages.push_back({
                    parser::OutputType::ERROR,
                    node->expressionToken(),
                    "Type mismatch in array assignment: array element is of type '" +
                    node->expressionType().value()->name() + "', but assigned expression is of type '" +
                    node->value()->expressionType().value()->name() + "'."
                });
                return;
            }
        } else {
            context.messages.push_back({
                parser::OutputType::ERROR,
                node->expressionToken(),
                "Could not determine types in array assignment."
            });
            return;
        }
    }

    void type_check(ast::ArrayAccess *node, Context &context) {
        type_check_base(node->accessExpression(), context);
        const auto varType = node->accessExpression()->expressionType();
        if (!varType) {
            context.messages.push_back({
                parser::OutputType::ERROR,
                node->expressionToken(),
                "Type of the array access expression could not be determined."
            });
            return;
        }
        node->setArrayType(varType.value());
        if (const auto arrayType = dynamic_cast<ArrayType *>(varType.value().get())) {
            node->setExpressionType(arrayType->baseType());
        } else if (const auto pointerType = dynamic_cast<PointerType *>(varType.value().get())) {
            node->setExpressionType(pointerType->baseType());
        } else {
            context.messages.push_back({
                parser::OutputType::ERROR,
                node->expressionToken(),
                "Type '" + varType.value()->name() + "' is not an array or pointer type."
            });
            return;
        }
    }

    void type_check(ast::VariableAccess *node, Context &context) {
        if (node->expressionToken().lexical() == "_")
            return;
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
            node->setConstant(it->second->constant);
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
            if (node->lhs()->expressionType())
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
        } else if (node->functionName() == "printf") {
            if (node->args().empty()) {
                context.messages.push_back({
                    parser::OutputType::ERROR,
                    node->expressionToken(),
                    "'printf' expects at least one argument."
                });
                return;
            }
            // 'println' can accept any type for now
            node->setExpressionType(context.registry.getTypeByName("void").value());
            return;
        }

        for (const auto funcDef: context.findFunctionsByName(node->modulePathName(), node->functionName())) {
            const auto oldVariables = context.currentVariables;
            type_check_base(funcDef, context);
            context.currentVariables = oldVariables;
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
                        !funcDef->args()[i].type ||
                        *funcDef->args()[i].type.value() != *node->args()[i]->expressionType().value()
                    ) {
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

    void type_check(const ast::ReturnStatement *node, Context &context) {
        if (node->returnValue()) {
            type_check_base(node->returnValue().value(), context);
        }
    }


    void type_check(ast::VariableDeclaration *node, Context &context) {
        const auto varType = resolveFromRawType(node->type(), context.registry);
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
        if (context.currentVariables.contains(node->expressionToken().lexical())) {
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
                    "Could not determine type of initial value for variable '" + node->expressionToken().
                    lexical() +
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


    void type_check(const ast::VariableAssignment *node, Context &context) {
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
        if (node->expressionType().has_value())
            return;
        // Example type checking logic for a function definition
        if (node->functionName() == "main" && !node->args().empty()) {
            context.messages.push_back({
                parser::OutputType::ERROR,
                node->expressionToken(),
                "The 'main' function must not have parameters."
            });
        }

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

        for (auto &arg: node->args()) {
            arg.type = resolveFromRawType(arg.rawType.get(), context.registry);
            if (!arg.type) {
                context.messages.push_back({
                    parser::OutputType::ERROR,
                    node->expressionToken(),
                    "Unknown type '" + arg.rawType->typeToken.lexical() + "' for argument '" + arg.name.lexical() + "'."
                });
            }
            context.currentVariables.emplace(arg.name.lexical(), Variable{
                                                 arg.name.lexical(), arg.type.value_or(nullptr), arg.isConstant
                                             });
        }
        for (auto &stmt: node->statements()) {
            type_check_base(stmt.get(), context);
            if (const auto returnStatement = dynamic_cast<ast::ReturnStatement *>(stmt.get())) {
                if (node->returnType()) {
                    if (returnStatement->returnValue()) {
                        if (!returnStatement->returnValue().value()->expressionType()) {
                            context.messages.push_back({
                                parser::OutputType::ERROR,
                                returnStatement->returnValue().value()->expressionToken(),
                                "Could not determine type of return value in function '" + node->functionName() + "'."
                            });
                        } else if (*returnStatement->returnValue().value()->expressionType().value() != *node->
                                   expressionType().value()) {
                            context.messages.push_back({
                                parser::OutputType::ERROR,
                                returnStatement->returnValue().value()->expressionToken(),
                                "Type mismatch in return statement of function '" + node->functionName() +
                                "': expected '" +
                                resolveFromRawType(node->returnType().value(), context.registry).value()->name() +
                                "', but got '" +
                                returnStatement->returnValue().value()->expressionType().value()->name() + "'."
                            });
                        }
                    } else {
                        context.messages.push_back({
                            parser::OutputType::ERROR,
                            returnStatement->expressionToken(),
                            "Return statement in function '" + node->functionName() +
                            "' must return a value of type '" +
                            resolveFromRawType(node->returnType().value(), context.registry).value()->name() + "'."
                        });
                    }
                } else {
                    if (returnStatement->returnValue()) {
                        context.messages.push_back({
                            parser::OutputType::ERROR,
                            returnStatement->returnValue().value()->expressionToken(),
                            "Return statement in void function '" + node->functionName() + "' must not return a value."
                        });
                    }
                }
            }
        }
        context.currentVariables.clear();


        // Further type checking logic would go here...
    }

    void type_check(ast::ExternFunctionDefinition *node, Context &context) {
        // Example type checking logic for a function definition


        for (auto &arg: node->args()) {
            arg.type = resolveFromRawType(arg.rawType.get(), context.registry);
            if (!arg.type) {
                context.messages.push_back({
                    parser::OutputType::ERROR,
                    node->expressionToken(),
                    "Unknown type '" + arg.rawType->typeToken.lexical() + "' for argument '" + arg.name.lexical() + "'."
                });
            }
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


    std::optional<int64_t> resolveEnumValue(const std::optional<std::unique_ptr<ast::ASTNode> > &value,
                                            Context &context) {
        if (!value)
            return std::nullopt;

        if (const auto intLiteral = dynamic_cast<ast::NumberConstant *>(value->get())) {
            return std::get<int64_t>(intLiteral->value());
        } else {
            context.messages.push_back({
                parser::OutputType::ERROR,
                value->get()->expressionToken(),
                "Enum variant value must be an integer literal."
            });
            return std::nullopt;
        }
    }

    void type_check(const std::shared_ptr<parser::Module> &module, Context &context, TypeCheckResult &result) {
        context.module = module;
        for (auto &childModule: module->modules) {
            type_check(childModule, context, result);
        }

        context.module = module;
        // resolve global type declarations first
        for (const auto &node: module->nodes) {
            if (const auto structDecl = dynamic_cast<ast::StructDeclaration *>(node.get())) {
                std::vector<types::StructField> structFields;
                for (const auto &[name, type]: structDecl->fields()) {
                    const auto fieldType = resolveFromRawType(type.get(), context.registry);
                    if (!fieldType) {
                        context.messages.push_back({
                            parser::OutputType::ERROR,
                            type->typeToken,
                            "Unknown type '" + type->typeToken.lexical() + "' for field '" + name.lexical() +
                            "' in struct '" + structDecl->expressionToken().lexical() + "'."
                        });
                        continue;
                    }
                    structFields.push_back({
                        .type = fieldType.value(),
                        .name = name.lexical()
                    });
                }
                std::vector<ast::FunctionDefinitionBase *> methods;
                for (const auto &method: structDecl->methods()) {
                    methods.push_back(method.get());
                }
                auto type = std::make_shared<types::StructType>(structDecl->expressionToken().lexical(),
                                                                structFields, methods);
                context.registry.registerType(type);
            } else if (const auto enumDecl = dynamic_cast<ast::EnumDeclaration *>(node.get())) {
                std::vector<EnumVariant> enumVariants;
                int64_t nextValue = 0;
                for (const auto &value: enumDecl->variants()) {
                    const auto resolvedValue = resolveEnumValue(value.value, context).value_or(nextValue);
                    enumVariants.push_back(EnumVariant{
                        .name = value.name.lexical(),
                        .value = resolvedValue
                    });
                    nextValue = resolvedValue + 1;
                }
                auto type = std::make_shared<types::EnumType>(enumDecl->expressionToken().lexical(),
                                                              enumVariants);
                context.registry.registerType(type);
            }
        }
        for (auto &node: module->nodes) {
            type_check_base(node.get(), context);
        }

        for (const auto &node: module->functions) {
            type_check_base(node.get(), context);
        }
    }

    void type_check(const std::shared_ptr<parser::Module> &module, TypeCheckResult &result) {
        Context context;
        type_check(module, context, result);
        result.messages = std::move(context.messages);
    }
}
