//
// Created by stefan on 29.08.25.
//

#include  "parser/Parser.h"

#include <cassert>
#include <iomanip>
#include <iostream>

#include "ast/ArrayAccess.h"
#include "ast/ArrayAssignment.h"
#include "ast/ArrayInitializer.h"
#include "ast/BinaryExpression.h"
#include "ast/BreakStatement.h"
#include "ast/Comparisson.h"
#include "ast/ExternFunctionDefinition.h"
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
#include "ast/UseModule.h"
#include "ast/VariableAccess.h"
#include "ast/VariableAssignment.h"
#include "ast/VariableDeclaration.h"
#include "ast/WhileLoop.h"
#include "magic_enum/magic_enum.hpp"

namespace parser {
    std::string outputTypeString(OutputType outputType) {
        switch (outputType) {
            case OutputType::ERROR:
                return "error";
            case OutputType::WARN:
                return "warn";
            case OutputType::HINT:
                return "info";
        }
        assert(false && "Unknown output type");
        return "unknown";
    }

    Color::Modifier outputTypeToColor(OutputType outputType) {
        switch (outputType) {
            case OutputType::ERROR:
                return Color::FG_RED;
            case OutputType::WARN:
                return Color::FG_GREEN;
            case OutputType::HINT:
                return Color::FG_BLUE;
        }
        assert(false && "Unknown output type");
        return Color::FG_DEFAULT;
    }

    void ParserMessasge::msg(std::ostream &ostream, bool printColor) const {
        if (printColor)
            ostream << token.source_location.filename << ":" << token.source_location.row << ":" << token.
                    source_location.col << ": "
                    << outputTypeToColor(outputType) << outputTypeString(outputType) << Color::Modifier(
                        Color::FG_DEFAULT)
                    << ": " << message << "\n";
        else
            ostream << token.source_location.filename << ":" << token.source_location.row << ":" << token.
                    source_location.col << ": "
                    << outputTypeString(outputType) << ": " << message << "\n";

        ostream << token.source_location.sourceline() << "\n";
        const size_t startOffset = token.source_location.byte_offset - token.source_location.lineStart() + 1;
        size_t endOffset = (token.source_location.source->find('\n', token.source_location.byte_offset) - 1) -
                           (token.source_location.byte_offset - 1);

        ostream << std::setw(startOffset) << std::setfill(' ') << '^' << std::setw(endOffset) << std::setfill('-') <<
                "\n";
    }

    class Parser {
    private:
        size_t m_current = 0;
        std::vector<Token> m_tokens;
        std::vector<ParserMessasge> m_messages;

    public:
        explicit Parser(std::vector<Token> tokens) : m_tokens(std::move(tokens)) {
        }

        std::optional<std::unique_ptr<ast::ASTNode> > parseNumber() {
            if (!canConsume(Token::NUMBER)) {
                return std::nullopt;
            }
            Token numberToken = current();
            consume(Token::NUMBER);
            if (numberToken.lexical().find('.') != std::string::npos) {
                return std::make_unique<ast::NumberConstant>(numberToken, ast::NumberType::FLOAT);
            } else {
                return std::make_unique<ast::NumberConstant>(numberToken, ast::NumberType::INTEGER);
            }
            return std::nullopt;
        }

        std::optional<std::unique_ptr<ast::ASTNode> > parseChar() {
            if (!canConsume(Token::CHAR)) {
                return std::nullopt;
            }
            Token charToken = current();
            consume(Token::CHAR);
            return std::make_unique<ast::NumberConstant>(charToken, ast::NumberType::CHAR);
        }

        std::optional<std::unique_ptr<ast::ASTNode> > parseString() {
            if (!canConsume(Token::STRING)) {
                return std::nullopt;
            }
            Token stringToken = current();
            consume(Token::STRING);
            return std::make_unique<ast::StringConstant>(stringToken);
        }

        std::optional<std::unique_ptr<ast::ReturnStatement> > parseReturnStatement() {
            if (!canConsumeKeyWord("return"))
                return std::nullopt;

            consumeKeyWord("return");
            auto returnToken = current();
            auto returnValue = parseExpression(true);
            return std::make_unique<ast::ReturnStatement>(returnToken, std::move(returnValue));
        }

        std::optional<std::unique_ptr<ast::ASTNode> > parseArrayInitializer(bool allowInit) {
            if (!canConsume(Token::LEFT_SQUAR) || !allowInit) {
                return std::nullopt;
            }
            Token startToken = current();
            consume(Token::LEFT_SQUAR);
            std::vector<std::unique_ptr<ast::ASTNode> > elements;
            while (!canConsume(Token::RIGHT_SQUAR) && !canConsume(Token::END_OF_FILE)) {
                if (auto element = parseExpression(true)) {
                    elements.push_back(std::move(element.value()));
                }
                if (canConsume(Token::COMMA)) {
                    consume(Token::COMMA);
                } else {
                    break;
                }
            }
            if (!canConsume(Token::RIGHT_SQUAR)) {
                m_messages.push_back(ParserMessasge{
                    .outputType = OutputType::ERROR,
                    .token = current(),
                    .message = "expected ']' at the end of array initializer",
                });
                return std::nullopt;
            }
            consume(Token::RIGHT_SQUAR);
            return std::make_unique<ast::ArrayInitializer>(startToken, std::move(elements));
        }

        bool canParseTypeCast() const {
            return canConsumeKeyWord("as");
        }

        std::optional<std::unique_ptr<ast::ASTNode> > tryParseTypeCast(
            std::optional<std::unique_ptr<ast::ASTNode> > value) {
            if (!canParseTypeCast()) {
                return std::move(value);
            }
            Token asToken = current();
            consumeKeyWord("as");
            auto rawType = parseRawType();
            if (!rawType) {
                m_messages.push_back(ParserMessasge{
                    .outputType = OutputType::ERROR,
                    .token = current(),
                    .message = "expected type name after 'as' for type cast",
                });
                return std::nullopt;
            }

            return std::make_unique<ast::TypeCast>(asToken, std::move(rawType.value()), std::move(value.value()));
        }

        std::optional<std::unique_ptr<ast::ASTNode> > parseStructInitialization(bool allowInit) {
            if (!canConsume(Token::IDENTIFIER) || !canConsume(Token::OPEN_BRACE, 1) || !allowInit) {
                return std::nullopt;
            }
            auto structNameToken = current();
            consume(Token::IDENTIFIER);
            consume(Token::OPEN_BRACE);
            std::vector<ast::StructInitField> fields;
            while (!canConsume(Token::CLOSE_BRACE) && !canConsume(Token::END_OF_FILE)) {
                if (!canConsume(Token::IDENTIFIER)) {
                    m_messages.push_back(ParserMessasge{
                        .outputType = OutputType::ERROR,
                        .token = current(),
                        .message = "expected field name in struct initialization",
                    });
                    return std::nullopt;
                }
                auto fieldNameToken = current();
                consume(Token::IDENTIFIER);
                if (!canConsume(Token::COLON)) {
                    m_messages.push_back(ParserMessasge{
                        .outputType = OutputType::ERROR,
                        .token = current(),
                        .message = "expected ':' after field name in struct initialization",
                    });
                    return std::nullopt;
                }
                consume(Token::COLON);
                auto fieldValue = parseExpression(false);
                if (!fieldValue) {
                    m_messages.push_back(ParserMessasge{
                        .outputType = OutputType::ERROR,
                        .token = current(),
                        .message = "expected expression for field value in struct initialization",
                    });
                    return std::nullopt;
                }
                fields.push_back(ast::StructInitField{
                    .name = fieldNameToken,
                    .value = std::move(fieldValue.value()),
                });
                if (!canConsume(Token::COMMA)) {
                    break;
                }
                tryConsume(Token::COMMA);
            }
            consume(Token::CLOSE_BRACE);

            return std::make_unique<ast::StructInitialization>(structNameToken, std::move(fields));
        }

        std::optional<std::unique_ptr<ast::ASTNode> > tryParseToken(bool allowInit = false) {
            std::optional<std::unique_ptr<ast::ASTNode> > result = std::nullopt;
            if (auto number = parseNumber()) {
                result = std::move(number.value());
            } else if (auto string = parseString()) {
                result = std::move(string.value());
            } else if (auto character = parseChar()) {
                result = std::move(character.value());
            } else if (auto functionCall = parseFunctionCall()) {
                result = std::move(functionCall.value());
            } else if (auto arrayAccess = parseArrayAccess()) {
                result = std::move(arrayAccess.value());
            } else if (auto structInitilization = parseStructInitialization(allowInit)) {
                result = std::move(structInitilization.value());
            } else if (auto varAccess = parseVariableAccess()) {
                result = std::move(varAccess.value());
            } else if (auto arrayInit = parseArrayInitializer(allowInit)) {
                result = std::move(arrayInit.value());
            }
            if (result) {
                if (canParseMemberAccess()) {
                    result = std::move(parseMemberAccess(std::move(result)).value());
                }
                if (canParseTypeCast()) {
                    result = std::move(tryParseTypeCast(std::move(result)).value());
                }
            }

            return result;
        }

        std::optional<std::unique_ptr<ast::ASTNode> > parseArrayAccess() {
            if (!canConsume(Token::IDENTIFIER) || !canConsume(Token::LEFT_SQUAR, 1)) {
                return std::nullopt;
            }
            auto lhs = current();
            consume(Token::IDENTIFIER);
            consume(Token::LEFT_SQUAR);
            auto indexExpr = parseExpression(false);
            if (!indexExpr || !indexExpr.value()) {
                m_messages.push_back(ParserMessasge{
                    .outputType = OutputType::ERROR,
                    .token = current(),
                    .message = "expected expression inside array access '[]'",
                });
                return std::nullopt;
            }
            if (!canConsume(Token::RIGHT_SQUAR)) {
                m_messages.push_back(ParserMessasge{
                    .outputType = OutputType::ERROR,
                    .token = current(),
                    .message = "expected ']' at the end of array access",
                });
                return std::nullopt;
            }
            consume(Token::RIGHT_SQUAR);
            return parseExpression(false, std::make_unique<ast::ArrayAccess>(std::move(lhs),
                                                                             std::move(indexExpr.value())));
        }

        bool canParseMemberAccess() {
            return canConsume(Token::DOT) && canConsume(Token::IDENTIFIER, 1);
        }

        std::optional<std::unique_ptr<ast::ASTNode> > parseMemberAccess(
            std::optional<std::unique_ptr<ast::ASTNode> > origLhs) {
            if (!canParseMemberAccess()) {
                return std::nullopt;
            }

            consume(Token::DOT);
            Token funcNameToken = current();
            consume(Token::IDENTIFIER);
            if (!canConsume(Token::LEFT_CURLY)) {
                auto result = std::make_unique<ast::FieldAccess>(std::move(funcNameToken), std::move(origLhs.value()));
                if (canConsume(Token::DOT))
                    return parseMemberAccess(std::move(result));
                return std::move(result);
            }
            consume(Token::LEFT_CURLY);
            std::vector<std::unique_ptr<ast::ASTNode> > args;
            args.push_back(std::move(origLhs.value()));
            while (!canConsume(Token::RIGHT_CURLY) && !canConsume(Token::END_OF_FILE)) {
                if (auto arg = parseExpression(true)) {
                    args.push_back(std::move(arg.value()));
                }
                if (canConsume(Token::COMMA)) {
                    consume(Token::COMMA);
                } else {
                    break;
                }
            }
            if (!canConsume(Token::RIGHT_CURLY)) {
                m_messages.push_back(ParserMessasge{
                    .outputType = OutputType::ERROR,
                    .token = current(),
                    .message = "expected ')' at the end of function call",
                });
                return std::nullopt;
            }
            consume(Token::RIGHT_CURLY);
            std::vector<Token> namespacePrefix;

            return std::make_unique<ast::FunctionCallNode>(funcNameToken, namespacePrefix, std::move(args));
        }

        std::optional<std::unique_ptr<ast::ASTNode> > parseVariableAccess() {
            if (!canConsume(Token::IDENTIFIER)) {
                return std::nullopt;
            }
            Token nameToken = current();
            consume(Token::IDENTIFIER);

            return std::make_unique<ast::VariableAccess>(std::move(nameToken));
        }


        std::optional<std::unique_ptr<ast::ASTNode> > parseBaseExpression(bool allowInit,
                                                                          std::optional<std::unique_ptr<ast::ASTNode> >
                                                                          origLhs = std::nullopt,
                                                                          bool includeCompare = true) {
            auto lhs = (origLhs.has_value()) ? std::move(origLhs) : tryParseToken(allowInit);


            if (lhs) {
                Token operatorToken = current();

                if (tryConsume(Token::PLUS)) {
                    auto rhs = tryParseToken(allowInit);

                    if (canConsume(Token::MUL) or canConsume(Token::DIV) or canConsume(Token::LEFT_CURLY)) {
                        rhs = parseBaseExpression(allowInit, std::move(rhs), false);
                    }
                    if (!rhs || !rhs.value()) {
                        m_messages.push_back(ParserMessasge{
                            .token = current(),
                            .message = "missing right hand side expression after '+' operator",
                        });
                        return lhs;
                    }

                    return parseExpression(false,
                                           std::make_unique<ast::BinaryExpression>(
                                               operatorToken, ast::BinaryOperator::ADD,
                                               std::move(lhs.value()),
                                               std::move(rhs.value())));
                }
                if (tryConsume(Token::MINUS)) {
                    auto rhs = tryParseToken();
                    if (canConsume(Token::MUL) or canConsume(Token::DIV) or canConsume(Token::LEFT_CURLY)) {
                        rhs = parseBaseExpression(false, std::move(rhs), false);
                    }

                    return parseExpression(false,
                                           std::make_unique<ast::BinaryExpression>(
                                               operatorToken, ast::BinaryOperator::SUB,
                                               std::move(lhs.value()), std::move(rhs.value())));
                }
                if (tryConsume(Token::MUL)) {
                    auto rhs = tryParseToken();
                    return parseExpression(false,
                                           std::make_unique<ast::BinaryExpression>(
                                               operatorToken, ast::BinaryOperator::MUL,
                                               std::move(lhs.value()), std::move(rhs.value())));
                }
                if (tryConsume(Token::DIV)) {
                    auto rhs = tryParseToken();
                    return parseExpression(false,
                                           std::make_unique<ast::BinaryExpression>(
                                               operatorToken, ast::BinaryOperator::DIV,
                                               std::move(lhs.value()), std::move(rhs.value())));
                }
                if (tryConsume(Token::PERCENT)) {
                    auto rhs = tryParseToken();
                    return parseExpression(false,
                                           std::make_unique<ast::BinaryExpression>(
                                               operatorToken, ast::BinaryOperator::MOD,
                                               std::move(lhs.value()), std::move(rhs.value())));
                }
            }

            if (canConsume(Token::LEFT_CURLY)) {
                consume(Token::LEFT_CURLY);
                auto result = parseExpression(allowInit, std::nullopt);
                consume(Token::RIGHT_CURLY);
                if (lhs.has_value())
                    if (auto binOp = dynamic_cast<ast::BinaryExpression *>(lhs.value().get())) {
                        result = std::make_unique<ast::BinaryExpression>(binOp->expressionToken(), binOp->binoperator(),
                                                                         binOp->movelhs(),
                                                                         std::move(result.value()));
                    }
                return parseExpression(allowInit, std::move(result));
            }

            if (includeCompare) {
                if (canConsume(Token::GREATER)) {
                    consume(Token::GREATER);
                    auto operatorToken = current();
                    if (canConsume(Token::EQUAL)) {
                        consume(Token::EQUAL);
                        auto rhs = parseBaseExpression(allowInit);
                        return std::make_unique<ast::Comparisson>(operatorToken, ast::CMPOperator::GREATER_EQUAL,
                                                                  std::move(lhs.value()), std::move(rhs.value()));
                    }
                    auto rhs = parseBaseExpression(allowInit);
                    if (!rhs) {
                        m_messages.push_back(ParserMessasge{
                            .token = current(),
                            .message = "expected a right hand side after an '>'!"
                        });
                        return std::nullopt;
                    }
                    return parseExpression(false,
                                           std::make_unique<ast::Comparisson>(
                                               operatorToken, ast::CMPOperator::GREATER, std::move(lhs.value()),
                                               std::move(rhs.value())));
                }
                if (canConsume(Token::LESS)) {
                    consume(Token::LESS);
                    auto operatorToken = current();
                    if (canConsume(Token::EQUAL)) {
                        consume(Token::EQUAL);
                        auto rhs = parseBaseExpression(allowInit);
                        return std::make_unique<ast::Comparisson>(operatorToken, ast::CMPOperator::LESS_EQUAL,
                                                                  std::move(lhs.value()), std::move(rhs.value()));
                    } else if (canConsume(Token::GREATER)) {
                        consume(Token::GREATER);
                        auto rhs = parseBaseExpression(allowInit);
                        return parseExpression(false,
                                               std::make_unique<ast::Comparisson>(
                                                   operatorToken, ast::CMPOperator::NOT_EQUALS,
                                                   std::move(lhs.value()), std::move(rhs.value())));
                    }
                    auto rhs = parseBaseExpression(allowInit);
                    if (!rhs) {
                        m_messages.push_back(ParserMessasge{
                            .token = current(),
                            .message = "expected a right hand side after an '<'!"
                        });
                        return std::nullopt;
                    }
                    return parseExpression(false,
                                           std::make_unique<ast::Comparisson>(
                                               operatorToken, ast::CMPOperator::LESS, std::move(lhs.value()),
                                               std::move(rhs.value())));
                }

                if (canConsume(Token::BANG) && canConsume(Token::EQUAL, 2)) {
                    consume(Token::BANG);
                    consume(Token::EQUAL);
                    auto operatorToken = current();
                    auto rhs = parseBaseExpression(allowInit);
                    return parseExpression(false,
                                           std::make_unique<ast::Comparisson>(
                                               operatorToken, ast::CMPOperator::NOT_EQUALS,
                                               std::move(lhs.value()), std::move(rhs.value())));
                }

                if (canConsume(Token::EQUAL) && canConsume(Token::EQUAL, 1)) {
                    consume(Token::EQUAL);
                    consume(Token::EQUAL);
                    auto operatorToken = current();
                    auto rhs = parseBaseExpression(allowInit);
                    return parseExpression(false,
                                           std::make_unique<ast::Comparisson>(
                                               operatorToken, ast::CMPOperator::EQUALS, std::move(lhs.value()),
                                               std::move(rhs.value())));
                }
            }

            return lhs;
        }

        std::optional<std::unique_ptr<ast::ASTNode> > parseLogicalExpression(
            std::optional<std::unique_ptr<ast::ASTNode> > lhs) {
            if (canConsumeKeyWord("not")) {
                consumeKeyWord("not");
                auto token = current();
                auto rhs = parseExpression(false);
                return parseExpression(false,
                                       std::make_unique<ast::LogicalExpression>(token, ast::LogicalOperator::NOT,
                                           std::move(rhs.value())));
            }

            if (!lhs)
                return std::nullopt;

            if (canConsumeKeyWord("or")) {
                consumeKeyWord("or");
                auto token = current();
                auto rhs = parseExpression(false);
                return parseExpression(false,
                                       std::make_unique<ast::LogicalExpression>(token, ast::LogicalOperator::OR,
                                           std::move(lhs.value()),
                                           std::move(rhs.value())));
            }
            if (canConsumeKeyWord("and")) {
                consumeKeyWord("and");
                auto token = current();
                auto rhs = parseExpression(false);
                return parseExpression(false,
                                       std::make_unique<
                                           ast::LogicalExpression>(token, ast::LogicalOperator::AND,
                                                                   std::move(lhs.value()),
                                                                   std::move(rhs.value())));
            }
            return lhs;
        }

        std::optional<std::unique_ptr<ast::ASTNode> > parseExpression(const bool allowInit,
                                                                      std::optional<std::unique_ptr<ast::ASTNode> >
                                                                      origLhs = std::nullopt) {
            auto lhs = parseLogicalExpression(std::move(origLhs));
            if (!lhs)
                lhs = std::move(origLhs);

            if (!lhs)
                lhs = parseBaseExpression(allowInit);
            if (!lhs)
                return std::nullopt;

            if (auto rhs = parseLogicalExpression(parseBaseExpression(allowInit, std::move(lhs))))
                return rhs;

            return lhs;
        }

        std::optional<std::unique_ptr<ast::ASTNode> > parseVariableAssignment() {
            if (((!canConsume(Token::IDENTIFIER) || !canConsume(Token::EQUAL, 1))
                 && (!canConsume(Token::IDENTIFIER) || !canConsume(Token::LEFT_SQUAR, 1))
            )) {
                return std::nullopt;
            }
            Token nameToken = current();
            consume(Token::IDENTIFIER);

            bool isArrayAccess = false;
            std::optional<std::unique_ptr<ast::ASTNode> > accessNode = std::nullopt;
            if (tryConsume(Token::LEFT_SQUAR)) {
                isArrayAccess = true;
                if (!canConsume(Token::NUMBER) && !canConsume(Token::IDENTIFIER)) {
                    m_messages.push_back(ParserMessasge{
                        .token = current(),
                        .message = "expected number or identifier inside array access '[]' in variable assignment!"
                    });
                    return std::nullopt;
                }
                accessNode = tryParseToken();
                if (!tryConsume(Token::RIGHT_SQUAR)) {
                    m_messages.push_back(ParserMessasge{
                        .token = current(),
                        .message = "expected ']' at the end of array access in variable assignment!"
                    });
                    return std::nullopt;
                }
            }
            if (!tryConsume(Token::EQUAL)) {
                m_messages.push_back(ParserMessasge{
                    .token = nameToken,
                    .message = "expected '=' after variable name in assignment!"
                });
                return std::nullopt;
            }
            auto value = parseExpression(false);
            if (!value) {
                m_messages.push_back(ParserMessasge{
                    .token = current(),
                    .message = "expected expression after '=' in variable assignment!"
                });
                return std::nullopt;
            }
            consume(Token::SEMICOLON);
            if (isArrayAccess) {
                return std::make_unique<ast::ArrayAssignment>(std::move(nameToken), std::move(value.value()),
                                                              std::move(accessNode.value()));
            }

            return std::make_unique<ast::VariableAssignment>(std::move(nameToken), std::move(value.value()));
        }

        std::optional<std::unique_ptr<ast::ASTNode> > parseFieldAssignment() {
            if (!canConsume(Token::IDENTIFIER) || !canConsume(Token::DOT, 1) || !canConsume(Token::IDENTIFIER, 2)) {
                return std::nullopt;
            }

            auto varAccess = parseMemberAccess(parseVariableAccess());
            if (!varAccess) {
                return std::nullopt;
            }

            std::optional<std::unique_ptr<ast::ASTNode> > accessNode = std::nullopt;

            if (!tryConsume(Token::EQUAL)) {
                m_messages.push_back(ParserMessasge{
                    .token = varAccess.value()->expressionToken(),
                    .message = "expected '=' after variable name in assignment!"
                });
                return std::nullopt;
            }
            auto value = parseExpression(false);
            if (!value) {
                m_messages.push_back(ParserMessasge{
                    .token = current(),
                    .message = "expected expression after '=' in variable assignment!"
                });
                return std::nullopt;
            }
            consume(Token::SEMICOLON);


            return std::make_unique<ast::FieldAssignment>(std::move(varAccess.value()->expressionToken()),
                                                          std::move(varAccess.value()),
                                                          std::move(value.value()));
        }

        std::optional<std::unique_ptr<ast::RawType> > parseRawType() {
            bool isPointer = false;
            if (tryConsume(Token::MUL)) {
                isPointer = true;
            }
            Token typeToken = current();
            if (tryConsume(Token::IDENTIFIER)) {
                return std::make_unique<ast::RawType>(typeToken, isPointer);
            }
            if (tryConsume(Token::LEFT_SQUAR)) {
                typeToken = current();
                if (!canConsume(Token::IDENTIFIER)) {
                    m_messages.push_back(ParserMessasge{
                        .token = current(),
                        .message = "expected type identifier after '[' in array type declaration!"
                    });
                    return std::nullopt;
                }
                consume(Token::IDENTIFIER);
                consume(Token::SEMICOLON);
                if (!canConsume(Token::NUMBER)) {
                    m_messages.push_back(ParserMessasge{
                        .token = current(),
                        .message = "expected number after ';' in array type declaration!"
                    });
                    return std::nullopt;
                }
                Token sizeToken = current();
                consume(Token::NUMBER);
                consume(Token::RIGHT_SQUAR);
                size_t size = 0;
                try {
                    size = std::stoul(sizeToken.lexical());
                } catch (const std::exception &e) {
                    m_messages.push_back(ParserMessasge{
                        .token = sizeToken,
                        .message = "invalid array size '" + sizeToken.lexical() + "'!"
                    });
                    return std::nullopt;
                }
                return std::make_unique<ast::ArrayRawType>(typeToken, isPointer,
                                                           std::make_unique<ast::RawType>(typeToken, false), size);
            }
            return std::nullopt;
        }

        std::optional<std::unique_ptr<ast::ASTNode> > parseVariableDeclaration() {
            if (!canConsumeKeyWord("let")) {
                return std::nullopt;
            }
            consumeKeyWord("let");
            bool constant = true;
            if (tryConsumeKeyWord("mut")) {
                constant = false;
            }
            Token nameToken = current();
            consume(Token::Type::IDENTIFIER);

            consume(Token::Type::COLON);
            auto type = parseRawType();
            if (!type) {
                m_messages.push_back(ParserMessasge{
                    .token = current(),
                    .message = "expected type after ':' in variable declaration!"
                });
                return std::nullopt;
            }


            std::optional<std::unique_ptr<ast::ASTNode> > value = std::nullopt;
            if (tryConsume(Token::EQUAL)) {
                value = parseExpression(true);
                consume(Token::Type::SEMICOLON);
            } else {
                consume(Token::Type::SEMICOLON);
            }

            return std::make_unique<ast::VariableDeclaration>(std::move(nameToken), std::move(type.value()),
                                                              constant,
                                                              std::move(value));
        }

        std::optional<std::unique_ptr<ast::ASTNode> > parseIfCondition() {
            if (!canConsumeKeyWord("if")) {
                return std::nullopt;
            }
            Token ifToken = current();
            consumeKeyWord("if");

            auto condition = parseExpression(false);
            if (!condition) {
                m_messages.push_back(ParserMessasge{
                    .token = ifToken,
                    .message = "expected condition expression after 'if'!"
                });
                return std::nullopt;
            }
            auto ifBlock = parseBlock();
            std::vector<std::unique_ptr<ast::ASTNode> > elseBlock;
            if (tryConsumeKeyWord("else")) {
                if (auto elseIf = parseIfCondition()) {
                    elseBlock.push_back(std::move(elseIf.value()));
                } else {
                    elseBlock = parseBlock();
                }
            }
            return std::make_unique<ast::IfCondition>(ifToken, std::move(condition.value()), std::move(ifBlock),
                                                      std::move(elseBlock));
        }

        std::optional<std::unique_ptr<ast::ASTNode> > parseBreak() {
            if (!canConsumeKeyWord("break")) {
                return std::nullopt;
            }
            Token breakToken = current();
            consumeKeyWord("break");
            consume(Token::SEMICOLON);
            return std::make_unique<ast::BreakStatement>(breakToken);
        }

        std::optional<std::unique_ptr<ast::ASTNode> > parseForLoop() {
            if (!canConsumeKeyWord("for")) {
                return std::nullopt;
            }
            Token forToken = current();
            consumeKeyWord("for");
            auto isConstant = tryConsumeKeyWord("let");

            if (!canConsume(Token::IDENTIFIER)) {
                m_messages.push_back(ParserMessasge{
                    .token = current(),
                    .message = "expected identifier after 'for'!"
                });
                return std::nullopt;
            }
            Token iteratorToken = current();
            consume(Token::IDENTIFIER);
            if (!canConsumeKeyWord("in")) {
                m_messages.push_back(ParserMessasge{
                    .token = current(),
                    .message = "expected 'in' after iterator in 'for' loop!"
                });
                return std::nullopt;
            }
            consumeKeyWord("in");
            auto rangeStart = parseExpression(false);
            if (!rangeStart) {
                m_messages.push_back(ParserMessasge{
                    .token = current(),
                    .message = "expected start expression after 'in' in 'for' loop!"
                });
                return std::nullopt;
            }
            if (!canConsume(Token::RANGE)) {
                auto block = parseBlock();

                return std::make_unique<ast::ForLoop>(forToken, std::move(iteratorToken),
                                                      std::move(rangeStart.value()),
                                                      nullptr, isConstant, false, std::move(block));
            }
            consume(Token::RANGE);
            auto inclusive = tryConsume(Token::EQUAL);
            auto rangeEnd = parseExpression(false);
            if (!rangeEnd) {
                m_messages.push_back(ParserMessasge{
                    .token = current(),
                    .message = "expected end expression after '..' in 'for' loop!"
                });
                return std::nullopt;
            }
            auto block = parseBlock();
            return std::make_unique<ast::ForLoop>(forToken, std::move(iteratorToken), std::move(rangeStart.value()),
                                                  std::move(rangeEnd.value()), isConstant, inclusive,
                                                  std::move(block));
        }

        std::optional<std::unique_ptr<ast::ASTNode> > parseWhileLoop() {
            if (!canConsumeKeyWord("while")) {
                return std::nullopt;
            }
            Token whileToken = current();
            consumeKeyWord("while");
            auto condition = parseExpression(false);
            if (!condition) {
                m_messages.push_back(ParserMessasge{
                    .token = whileToken,
                    .message = "expected condition expression after 'while'!"
                });
                return std::nullopt;
            }
            auto block = parseBlock();
            return std::make_unique<ast::WhileLoop>(whileToken, std::move(condition.value()), std::move(block));
        }

        std::vector<std::unique_ptr<ast::ASTNode> > parseBlock() {
            consume(Token::Type::OPEN_BRACE);
            std::vector<std::unique_ptr<ast::ASTNode> > nodes;
            while (!canConsume(Token::CLOSE_BRACE)) {
                if (tryConsume(Token::SEMICOLON)) {
                } else if (auto functionCall = parseFunctionCall()) {
                    nodes.push_back(std::move(functionCall.value()));
                } else if (auto returnStatement = parseReturnStatement()) {
                    nodes.push_back(std::move(returnStatement.value()));
                } else if (auto varDecl = parseVariableDeclaration()) {
                    nodes.push_back(std::move(varDecl.value()));
                } else if (auto varAssign = parseVariableAssignment()) {
                    nodes.push_back(std::move(varAssign.value()));
                } else if (auto fieldAssign = parseFieldAssignment()) {
                    nodes.push_back(std::move(fieldAssign.value()));
                } else if (auto ifCondition = parseIfCondition()) {
                    nodes.push_back(std::move(ifCondition.value()));
                } else if (auto whileLoop = parseWhileLoop()) {
                    nodes.push_back(std::move(whileLoop.value()));
                } else if (auto forLoop = parseForLoop()) {
                    nodes.push_back(std::move(forLoop.value()));
                } else if (auto breakStmt = parseBreak()) {
                    nodes.push_back(std::move(breakStmt.value()));
                } else {
                    m_messages.push_back(ParserMessasge{
                        .token = current(),
                        .message = "unexpected token found " +
                                   std::string(magic_enum::enum_name(current().type)) + "!"
                    });
                    next();
                    if (current().type == Token::Type::END_OF_FILE) {
                        break;
                    }
                }
            }
            consume(Token::Type::CLOSE_BRACE);
            return nodes;
        }

        std::optional<ast::FunctionArgument> tryParseFunctionArgument() {
            if (!canConsume(Token::IDENTIFIER))
                return std::nullopt;
            const Token nameToken = current();
            consume(Token::Type::IDENTIFIER);
            consume(Token::Type::COLON);
            auto rawType = parseRawType();
            if (!rawType) {
                m_messages.push_back(ParserMessasge{
                    .token = current(),
                    .message = "expected type after ':' in function argument!"
                });
                return std::nullopt;
            }
            return ast::FunctionArgument(std::move(nameToken.lexical()),
                                         std::move(rawType.value())
            );
        }

        std::optional<std::unique_ptr<ast::ASTNode> > parseExternFunctionDefinition() {
            if (canConsumeKeyWord("extern")) {
                consumeKeyWord("extern");
                if (!canConsumeKeyWord("fn")) {
                    m_messages.push_back(ParserMessasge{
                        .token = current(),
                        .message = "expected 'fn' after 'extern'!"
                    });
                    return std::nullopt;
                }
                consumeKeyWord("fn");
                Token nameToken = current();
                if (!canConsume(Token::IDENTIFIER)) {
                    m_messages.push_back(ParserMessasge{
                        .token = current(),
                        .message = "expected a function name but found a '" + nameToken.lexical() + "'"
                    });
                    return std::nullopt;
                }
                consume(Token::Type::IDENTIFIER);

                consume(Token::Type::LEFT_CURLY);
                std::vector<ast::FunctionArgument> functionArgs;
                while (!canConsume(Token::Type::RIGHT_CURLY) && hasNext()) {
                    if (auto arg = tryParseFunctionArgument()) {
                        functionArgs.push_back(std::move(arg.value()));
                        tryConsume(Token::COMMA);
                    } else {
                        m_messages.push_back(ParserMessasge{
                            .token = current(),
                            .message = "a function argument but found '" + current().lexical() + "'"
                        });
                        break;
                    }
                    if (canConsume(Token::Type::RIGHT_CURLY))
                        break;
                }
                consume(Token::Type::RIGHT_CURLY);
                consume(Token::COLON);
                auto returnType = parseRawType();
                consume(Token::SEMICOLON);

                return std::make_unique<ast::ExternFunctionDefinition>(std::move(nameToken),
                                                                       std::move(functionArgs),
                                                                       std::move(returnType));
            }
            return std::nullopt;
        }

        std::optional<std::unique_ptr<ast::ASTNode> > parseFunctionDefinition() {
            if (!canConsumeKeyWord("fn")) {
                return std::nullopt;
            }
            consumeKeyWord("fn");
            Token nameToken = current();
            if (!canConsume(Token::IDENTIFIER)) {
                m_messages.push_back(ParserMessasge{
                    .token = current(),
                    .message = "expected a function name but found a '" + nameToken.lexical() + "'"
                });
                return std::nullopt;
            }
            consume(Token::Type::IDENTIFIER);

            consume(Token::Type::LEFT_CURLY);
            std::vector<ast::FunctionArgument> functionArgs;
            while (!canConsume(Token::Type::RIGHT_CURLY) && hasNext()) {
                if (auto arg = tryParseFunctionArgument()) {
                    functionArgs.push_back(std::move(arg.value()));
                    tryConsume(Token::COMMA);
                } else {
                    m_messages.push_back(ParserMessasge{
                        .token = current(),
                        .message = "a function argument but found '" + current().lexical() + "'"
                    });
                    break;
                }
                if (canConsume(Token::Type::RIGHT_CURLY))
                    break;
            }
            consume(Token::Type::RIGHT_CURLY);
            consume(Token::COLON);
            auto returnType = parseRawType();

            auto block = parseBlock();
            return std::make_unique<ast::FunctionDefinition>(std::move(nameToken), std::move(functionArgs),
                                                             std::move(returnType),
                                                             std::move(block));
        }

        std::optional<std::unique_ptr<ast::FunctionCallNode> > parseFunctionCall() {
            if ((!canConsume(Token::IDENTIFIER) || !canConsume(Token::Type::LEFT_CURLY, 1))
                && (!canConsume(Token::IDENTIFIER) || !canConsume(Token::Type::NS_SEPARATOR, 1))
            ) {
                return std::nullopt;
            }
            Token nameToken = current();
            std::vector<Token> namespacePrefix;
            consume(Token::Type::IDENTIFIER);
            while (canConsume(Token::NS_SEPARATOR)) {
                namespacePrefix.push_back(nameToken);
                consume(Token::Type::NS_SEPARATOR);
                nameToken = current();
                if (!canConsume(Token::IDENTIFIER)) {
                    m_messages.push_back(ParserMessasge{
                        .token = current(),
                        .message = "expected identifier after '::' in function call!"
                    });
                    return std::nullopt;
                }
                consume(Token::Type::IDENTIFIER);
            }
            consume(Token::Type::LEFT_CURLY);
            std::vector<std::unique_ptr<ast::ASTNode> > args;
            while (!canConsume(Token::Type::RIGHT_CURLY) && hasNext()) {
                if (auto arg = parseExpression(true)) {
                    args.push_back(std::move(arg.value()));
                    if (canConsume(Token::COMMA)) {
                        consume(Token::COMMA);
                    }
                } else {
                    m_messages.push_back(ParserMessasge{
                        .token = current(),
                        .message = "unexpected token found in function call arguments " +
                                   std::string(magic_enum::enum_name(current().type)) + "!"
                    });
                    next();
                }
            }
            consume(Token::Type::RIGHT_CURLY);
            return std::make_unique<ast::FunctionCallNode>(nameToken, namespacePrefix, std::move(args));
        }

        std::optional<std::unique_ptr<ast::ASTNode> > parseUseModule() {
            if (!canConsumeKeyWord("use")) {
                return std::nullopt;
            }
            consumeKeyWord("use");
            if (!canConsume(Token::IDENTIFIER)) {
                m_messages.push_back(ParserMessasge{
                    .token = current(),
                    .message = "expected module name after 'use'!"
                });
                return std::nullopt;
            }
            std::vector<Token> modulePath;
            while (canConsume(Token::IDENTIFIER)) {
                Token moduleName = current();
                consume(Token::IDENTIFIER);
                modulePath.push_back(moduleName);
                if (canConsume(Token::NS_SEPARATOR)) {
                    consume(Token::NS_SEPARATOR);
                } else {
                    break;
                }
            }
            consume(Token::SEMICOLON);
            return std::make_unique<ast::UseModule>(modulePath);
        }

        std::optional<std::unique_ptr<ast::ASTNode> > parseStructDeclaration() {
            if (!canConsumeKeyWord("struct")) {
                return std::nullopt;
            }
            consumeKeyWord("struct");
            if (!canConsume(Token::IDENTIFIER)) {
                m_messages.push_back(ParserMessasge{
                    .token = current(),
                    .message = "expected struct name after 'struct'!"
                });
                return std::nullopt;
            }
            Token structName = current();
            consume(Token::IDENTIFIER);
            consume(Token::OPEN_BRACE);
            std::vector<ast::StructField> fields;
            while (!canConsume(Token::CLOSE_BRACE) && hasNext()) {
                if (!canConsume(Token::IDENTIFIER)) {
                    m_messages.push_back(ParserMessasge{
                        .token = current(),
                        .message = "expected field name in struct declaration!"
                    });
                }
                auto fieldName = current();
                consume(Token::IDENTIFIER);
                consume(Token::COLON);
                auto type = parseRawType();
                if (!type) {
                    m_messages.push_back(ParserMessasge{
                        .token = current(),
                        .message = "expected type after field name in struct declaration!"
                    });
                    return std::nullopt;
                }

                fields.push_back(ast::StructField{.name = fieldName, .type = std::move(type.value())});
                if (!tryConsume(Token::COMMA)) {
                    break;
                }
            }
            consume(Token::CLOSE_BRACE);
            return std::make_unique<ast::StructDeclaration>(std::move(structName), std::move(fields));
        }

        ParseResult parse() {
            std::vector<std::unique_ptr<ast::ASTNode> > nodes;
            while (!canConsume(Token::END_OF_FILE) && hasNext()) {
                if (auto functionDef = parseFunctionDefinition()) {
                    nodes.push_back(std::move(functionDef.value()));
                } else if (auto externFnDefintion = parseExternFunctionDefinition()) {
                    nodes.push_back(std::move(externFnDefintion.value()));
                } else if (auto useModule = parseUseModule()) {
                    nodes.push_back(std::move(useModule.value()));
                } else if (auto structDecl = parseStructDeclaration()) {
                    nodes.push_back(std::move(structDecl.value()));
                } else {
                    m_messages.push_back(ParserMessasge{
                        .token = current(),
                        .message = "unexpected token found " +
                                   std::string(magic_enum::enum_name(current().type)) + "!"
                    });
                    next();
                }
            }

            return ParseResult{
                .nodes = std::move(nodes),
                .messages = m_messages
            };
        }

    private
    :
        Token next() {
            if (hasNext())
                ++m_current;
            return current();
        }

        Token current() {
            return m_tokens[m_current];
        }

        bool consume(const Token::Type Token) {
            if (canConsume(Token)) {
                ++m_current;
                return true;
            }

            m_messages.push_back(ParserMessasge{

                .token = m_tokens.at(m_current),
                .message = "expected token '" + std::string(magic_enum::enum_name(Token)) + "' but found " +
                           std::string(magic_enum::enum_name(m_tokens[m_current].type)) + "!"
            });

            return false;
        }

        bool tryConsume(const Token::Type Token) {
            if (canConsume(Token)) {
                ++m_current;
                return true;
            }
            return false;
        }

        [[nodiscard]] bool hasNext() const {
            return m_current < m_tokens.size() - 1;
        }

        [[nodiscard]] bool canConsume(const Token::Type Token) const {
            return canConsume(Token, 0);;
        }

        [[nodiscard]] bool canConsume(const Token::Type Token, const size_t next) const {
            return hasNext() && m_tokens[m_current + next].type == Token;
        }

        bool consumeKeyWord(const std::string &keyword) {
            if (tryConsumeKeyWord(keyword)) {
                return true;
            }
            m_messages.push_back(ParserMessasge{
                .token = m_tokens[m_current],
                .message = "expected keyword '" + keyword + "' but found " +
                           std::string(m_tokens[m_current].lexical()) + "!"
            });
            return false;
        }

        bool tryConsumeKeyWord(const std::string &keyword) {
            if (canConsumeKeyWord(keyword)) {
                next();
                return true;
            }
            return false;
        }

        [[nodiscard]] bool canConsumeKeyWord(const std::string &keyword) const {
            return canConsume(Token::Type::KEYWORD) && m_tokens[m_current].lexical() == keyword;
        }
    };

    ParseResult parse_tokens(const std::vector<Token> &tokens) {
        Parser parser(tokens);
        return parser.parse();
    }
}
