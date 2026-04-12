#include "types/TypeResolver.h"

#include "ast/BinaryExpression.h"
#include "ast/ReturnStatement.h"
#include "ast/VariableAssignment.h"


namespace types {
    std::unique_ptr<ast::RawType> rawTypeFromNumberConstant(ast::NumberConstant *node) {
        const auto source_location = node->expressionToken().source_location;
        std::string typeName;
        ast::TypeModifier typeModifier = ast::TypeModifier::NONE;
        switch (node->numberType()) {
            case ast::NumberType::INTEGER: {
                if (node->numBits() == 32) {
                    typeName = "i32";
                } else {
                    typeName = "i64";
                }
            }
            break;
            case ast::NumberType::HEX_NUMBER:
            case ast::NumberType::OCT_NUMBER:
            case ast::NumberType::BIN_NUMBER: {
                if (node->numBits() == 32) {
                    typeName = "u32";
                } else {
                    typeName = "u64";
                }
            }
            break;
            case ast::NumberType::FLOAT:
                typeName = "float";
                break;
            case ast::NumberType::DOUBLE:
                typeName = "double";
                break;
            case ast::NumberType::CHAR:
                typeName = "u8";
                break;
            case ast::NumberType::BOOLEAN:
                typeName = "bool";
                break;
            case ast::NumberType::NULLPTR:
                typeName = "void";
                typeModifier = ast::TypeModifier::POINTER;
                break;
            default:
                assert(false && "Unknown number type");
        }
        return std::make_unique<ast::RawType>(Token(typeName, Token::IDENTIFIER, source_location),
                                              typeModifier);
    }

    std::optional<std::unique_ptr<ast::RawType> > resolveRawTypeFromUsage(ast::ASTNode *node,
                                                                          const std::string &typeName) {
        switch (node->nodeType()) {
            case ast::NodeType::VARIABLE_ASSIGNMENT: {
                auto assignment = dynamic_cast<ast::VariableAssignment *>(node);
                if (assignment->expressionToken().lexical() == typeName) {
                    return resolveRawTypeFromUsage(assignment->expression(), typeName);
                }
                break;
            }
            case ast::NodeType::BINARY_EXPRESSION: {
                auto binaryExpr = dynamic_cast<ast::BinaryExpression *>(node);
                if (typeName.empty()) {
                    if (auto result = resolveRawTypeFromUsage(binaryExpr->lhs(), typeName)) {
                        return result;
                    }
                    if (auto result = resolveRawTypeFromUsage(binaryExpr->rhs(), typeName)) {
                        return result;
                    }
                }

                if (binaryExpr->lhs()->expressionToken().lexical() == typeName) {
                    return resolveRawTypeFromUsage(binaryExpr->rhs(), typeName);
                }
                if (binaryExpr->rhs()->expressionToken().lexical() == typeName) {
                    return resolveRawTypeFromUsage(binaryExpr->rhs(), typeName);
                }
                break;
            }
            case ast::NodeType::NUMBER_CONSTANT: {
                auto numberConstant = dynamic_cast<ast::NumberConstant *>(node);
                return rawTypeFromNumberConstant(numberConstant);
            }
            case ast::NodeType::RETURN: {
                auto returnNode = dynamic_cast<ast::ReturnStatement *>(node);
                if (returnNode->returnValue()) {
                    return resolveRawTypeFromUsage(returnNode->returnValue().value(), typeName);
                }
                break;
            }
            case ast::NodeType::STRING_CONSTANT:
            case ast::NodeType::RAW_STRING_CONSTANT:

            case ast::NodeType::VARIABLE_ACCESS:
            case ast::NodeType::REFERENCE_ACCESS:

            default:
                break;
        }
        return std::nullopt;
    }

    std::optional<std::unique_ptr<ast::RawType> > resolveRawTypeFromUsage(ast::BlockNode *blockNode,
                                                                          const std::string &typeName) {
        for (auto &stmt: blockNode->statements()) {
            if (auto result = resolveRawTypeFromUsage(stmt.get(), typeName)) return result;
        }
        return std::nullopt;
    }

    std::optional<std::unique_ptr<ast::RawType> > resolveRawReturnTypeFromUsage(ast::BlockNode *blockNode) {
        for (auto &stmt: blockNode->statements()) {
            if (stmt->nodeType() == ast::NodeType::RETURN) {
                auto returnNode = dynamic_cast<ast::ReturnStatement *>(stmt.get());
                if (returnNode->returnValue()) {
                    return resolveRawTypeFromUsage(returnNode->returnValue().value(), "");
                }
            }
        }
        return std::nullopt;
    }


    std::optional<std::shared_ptr<VariableType> > resolveFromRawType(ast::RawType *rawType,
                                                                     std::shared_ptr<Scope> &currentScope,
                                                                     bool resolveGeneric) {
        const bool useRawGenericName = rawType->genericParam.has_value() && resolveGeneric;
        if (const auto arrayType = dynamic_cast<ast::ArrayRawType *>(rawType)) {
            const auto baseType = resolveFromRawType(arrayType->baseType.get(), currentScope);
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
        } else if (auto sliceType = dynamic_cast<ast::SliceRawType *>(rawType)) {
            const auto baseType = resolveFromRawType(sliceType->baseType.get(), currentScope);
            if (!baseType) return std::nullopt;
            auto type = currentScope->getSliceType(baseType.value());
            if (rawType->typeModifier == ast::TypeModifier::POINTER) {
                if (!type) return std::nullopt;
                return types::TypeRegistry::getPointerType(type.value());
            } else if (rawType->typeModifier == ast::TypeModifier::REFERENCE) {
                if (!type) return std::nullopt;
                return types::TypeRegistry::getReferenceType(type.value());
            }
            return type;
        } else if (auto pointerType = dynamic_cast<ast::PointerRawType *>(rawType)) {
            const auto baseType = resolveFromRawType(pointerType->baseType.get(), currentScope);
            if (!baseType) return std::nullopt;
            return types::TypeRegistry::getPointerType(baseType.value());
        } else if (auto functionType = dynamic_cast<ast::FunctionRawType *>(rawType)) {
            // Function types are not supported as variable types
            std::vector<std::shared_ptr<VariableType> > argTypes;
            for (const auto &argRawType: functionType->argumentTypes) {
                const auto argType = resolveFromRawType(argRawType.get(), currentScope);
                if (!argType) return std::nullopt;
                argTypes.push_back(argType.value());
            }
            const auto returnType = resolveFromRawType(functionType->returnType.get(), currentScope);
            if (!returnType) return std::nullopt;
            std::string name = "fn(";
            for (size_t i = 0; i < argTypes.size(); ++i) {
                name += argTypes[i]->name();
                if (i < argTypes.size() - 1) {
                    name += ",";
                }
            }
            name += "):" + returnType.value()->name();
            return std::make_shared<FunctionType>(name, argTypes, returnType.value());
        } else if (rawType->typeModifier == ast::TypeModifier::POINTER) {
            const auto baseType = currentScope->getTypeByName(rawType->fullTypeName(), useRawGenericName);
            if (!baseType) return std::nullopt;
            return types::TypeRegistry::getPointerType(baseType.value());
        } else if (rawType->typeModifier == ast::TypeModifier::REFERENCE) {
            const auto baseType = currentScope->getTypeByName(rawType->fullTypeName(), useRawGenericName);
            if (!baseType) return std::nullopt;
            return types::TypeRegistry::getReferenceType(baseType.value());
        } else {
            if (useRawGenericName) {
                auto nonGenericType = currentScope->getTypeByName(rawType->fullTypeName(), false);
                if (nonGenericType)
                    return nonGenericType;
                auto param = currentScope->getTypeByName(rawType->genericParam.value().lexical(), false);
                auto type = currentScope->getTypeByName(rawType->typeToken.lexical(), useRawGenericName);
                nonGenericType = type.value()->makeNonGenericType(param.value());
                currentScope->registerType(nonGenericType.value());
                return nonGenericType;
            } else
                return currentScope->getTypeByName(rawType->fullTypeName(), useRawGenericName);
        }
    }
}
