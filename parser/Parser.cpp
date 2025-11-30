

#include  "parser/Parser.h"

#include <cassert>
#include <iomanip>
#include "ast/ArrayAccess.h"
#include "ast/ArrayAssignment.h"
#include "ast/ArrayInitializer.h"
#include "ast/ArrayRepeatInitializer.h"
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

    void ParserMessasge::msg(std::ostream &ostream, const bool printColor) const {
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

        std::optional<std::unique_ptr<ast::ASTNode> > parseBoolean() {
            if (!canConsumeKeyWord("true") && !canConsumeKeyWord("false")) {
                return std::nullopt;
            }
            Token boolToken = current();
            consume(Token::KEYWORD);
            return std::make_unique<ast::NumberConstant>(boolToken, ast::NumberType::BOOLEAN);
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
                } else if (canConsume(Token::SEMICOLON)) {
                    consume(Token::SEMICOLON);
                    auto repeatCountNode = parseExpression(true);
                    if (!repeatCountNode) {
                        m_messages.push_back(ParserMessasge{
                            .outputType = OutputType::ERROR,
                            .token = current(),
                            .message = "expected expression for array initializer repeat count",
                        });
                        return std::nullopt;
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
                    return std::make_unique<ast::ArrayRepeatInitializer>(
                        startToken,
                        std::move(elements.back()),
                        std::move(repeatCountNode.value())
                    );
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

        [[nodiscard]] bool canParseTypeCast() const {
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

        bool isStructInitializationAhead() const {
            int lookaheadIndex = 0;
            if (!canConsume(Token::IDENTIFIER)) {
                return false;
            }
            lookaheadIndex++;
            if (canConsume(Token::LESS, lookaheadIndex)) {
                lookaheadIndex++;
                // Skip generic parameters
                while (canConsume(Token::IDENTIFIER, lookaheadIndex) || canConsume(Token::COMMA, lookaheadIndex)) {
                    lookaheadIndex++;
                }
                if (!canConsume(Token::GREATER, lookaheadIndex)) {
                    return false;
                }
                lookaheadIndex++;
            }
            return canConsume(Token::OPEN_BRACE, lookaheadIndex);
        }

        std::optional<std::unique_ptr<ast::ASTNode> > parseStructInitialization(bool allowInit) {
            if (!isStructInitializationAhead() || !allowInit) {
                return std::nullopt;
            }
            auto structNameToken = current();
            consume(Token::IDENTIFIER);

            std::optional<Token> genericParam = std::nullopt;
            if (tryConsume(Token::LESS)) {
                genericParam = current();
                consume(Token::IDENTIFIER);


                if (!canConsume(Token::GREATER)) {
                    m_messages.push_back(ParserMessasge{
                        .token = current(),
                        .message = "expected '>' to close generic struct declaration!"
                    });
                    return std::nullopt;
                }
                consume(Token::GREATER);
            }

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

            return std::make_unique<ast::StructInitialization>(structNameToken, genericParam, std::move(fields));
        }

        std::optional<std::unique_ptr<ast::ASTNode> > parseReferenceAccess() {
            if (!canConsume(Token::AND)) {
                return std::nullopt;
            }
            consume(Token::AND);
            auto refNode = tryParseToken(false);
            if (!refNode) {
                m_messages.push_back(ParserMessasge{
                    .outputType = OutputType::ERROR,
                    .token = current(),
                    .message = "expected expression after '&' for reference access",
                });
                return std::nullopt;
            }


            return std::make_unique<ast::ReferenceAccess>(refNode.value()->expressionToken(),
                                                          std::move(refNode.value()));
        }

        [[nodiscard]] bool canParseRange() const {
            return canConsume(Token::RANGE);
        }

        std::optional<std::unique_ptr<ast::ASTNode> > parseRange(std::optional<std::unique_ptr<ast::ASTNode> > lhs) {
            if (!canParseRange() && !lhs) {
                return std::nullopt;
            }
            auto startToken = lhs.value()->expressionToken();
            auto startExpr = std::move(lhs);
            if (!startExpr) {
                m_messages.push_back(ParserMessasge{
                    .outputType = OutputType::ERROR,
                    .token = current(),
                    .message = "expected expression before '..' for range",
                });
                return std::nullopt;
            }
            consume(Token::RANGE);
            auto inclusive = tryConsume(Token::EQUAL);
            auto endExpr = parseExpression(false);
            if (!endExpr) {
                m_messages.push_back(ParserMessasge{
                    .outputType = OutputType::ERROR,
                    .token = current(),
                    .message = "expected expression after '..' for range",
                });
                return std::nullopt;
            }
            return std::make_unique<ast::RangeExpression>(startToken, std::move(startExpr.value()),
                                                          std::move(endExpr.value()), inclusive);
        }

        std::optional<std::unique_ptr<ast::ASTNode> > parseEnumAccess() {
            if (!canConsume(Token::IDENTIFIER) or !canConsume(Token::NS_SEPARATOR, 1) or !canConsume(
                    Token::IDENTIFIER, 2)) {
                return std::nullopt;
            }
            Token enumNameToken = current();
            consume(Token::IDENTIFIER);
            consume(Token::NS_SEPARATOR);
            Token variantNameToken = current();
            consume(Token::IDENTIFIER);
            return std::make_unique<ast::EnumAccess>(enumNameToken, variantNameToken);
        }

        std::optional<std::unique_ptr<ast::ASTNode> > parseNull() {
            if (!canConsumeKeyWord("null")) {
                return std::nullopt;
            }
            Token nullToken = current();
            consumeKeyWord("null");
            return std::make_unique<ast::NumberConstant>(nullToken, ast::NumberType::NULLPTR);
        }

        std::optional<std::unique_ptr<ast::ASTNode> > tryParseToken(bool allowInit = false) {
            std::optional<std::unique_ptr<ast::ASTNode> > result = std::nullopt;

            if (auto number = parseNumber()) {
                result = std::move(number.value());
            } else if (auto string = parseString()) {
                result = std::move(string.value());
            } else if (auto character = parseChar()) {
                result = std::move(character.value());
            } else if (auto boolean = parseBoolean()) {
                result = std::move(boolean.value());
            } else if (auto nullPointer = parseNull()) {
                result = std::move(nullPointer.value());
            } else if (auto functionCall = parseFunctionCall()) {
                result = std::move(functionCall.value());
            } else if (auto enumAccess = parseEnumAccess()) {
                result = std::move(enumAccess.value());
            } else if (auto referenceAccess = parseReferenceAccess()) {
                result = std::move(referenceAccess.value());
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

                if (canParseArrayAccess()) {
                    result = std::move(parseArrayAccess(std::move(result)).value());
                }
                if (canParseRange()) {
                    result = std::move(parseRange(std::move(result)).value());
                }
            }

            return result;
        }


        std::optional<std::unique_ptr<ast::ASTNode> > parseArrayAccess(
            std::optional<std::unique_ptr<ast::ASTNode> > value) {
            if (!canConsume(Token::LEFT_SQUAR)) {
                return std::nullopt;
            }

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
            return parseExpression(false, std::make_unique<ast::ArrayAccess>(
                                       value.value()->expressionToken(), std::move(value.value()),
                                       std::move(indexExpr.value())));
        }

        [[nodiscard]] bool canParseMemberAccess() const {
            return canConsume(Token::DOT) && canConsume(Token::IDENTIFIER, 1);
        }

        [[nodiscard]] bool canParseArrayAccess() const {
            return canConsume(Token::LEFT_SQUAR);
        }

        std::optional<std::unique_ptr<ast::ASTNode> > parseMemberAccess(
            std::optional<std::unique_ptr<ast::ASTNode> > origLhs) {
            if (!canParseMemberAccess()) {
                return origLhs;
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
            auto instanceNode = std::make_unique<ast::ReferenceAccess>(origLhs.value()->expressionToken(),
                                                                       std::move(origLhs.value()));
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
                    .message = "expected ')' at the end of a method call",
                });
                return std::nullopt;
            }
            consume(Token::RIGHT_CURLY);
            std::vector<Token> namespacePrefix;

            return std::make_unique<ast::MethodCallNode>(funcNameToken, std::move(instanceNode), std::move(args));
        }

        std::optional<std::unique_ptr<ast::ASTNode> > parseVariableAccess() {
            if (!canConsume(Token::IDENTIFIER)) {
                return std::nullopt;
            }
            auto nameToken = current();
            consume(Token::IDENTIFIER);

            return std::make_unique<ast::VariableAccess>(std::move(nameToken));
        }


        std::optional<std::unique_ptr<ast::ASTNode> > parseBaseExpression(bool allowInit,
                                                                          std::optional<std::unique_ptr<ast::ASTNode> >
                                                                          origLhs = std::nullopt,
                                                                          bool includeCompare = true) {
            auto lhs = (origLhs.has_value()) ? std::move(origLhs) : tryParseToken(allowInit);


            if (lhs) {
                auto operatorToken = current();

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
                        result = std::make_unique<ast::BinaryExpression>(
                            binOp->expressionToken(), binOp->binoperator(),
                            binOp->movelhs(),
                            std::move(result.value()));
                    }
                return parseExpression(allowInit, std::move(result));
            }

            if (includeCompare && lhs) {
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

                if (canConsume(Token::BANG) && canConsume(Token::EQUAL, 1)) {
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
            if (!canConsume(Token::IDENTIFIER)) {
                return std::nullopt;
            }
            Token nameToken = current();
            auto varAccess = parseMemberAccess(parseVariableAccess());
            if (!varAccess) {
                return std::nullopt;
            }
            bool isArrayAccess = false;
            std::optional<std::unique_ptr<ast::ASTNode> > indexNode = std::nullopt;
            if (tryConsume(Token::LEFT_SQUAR)) {
                isArrayAccess = true;
                if (!canConsume(Token::NUMBER) && !canConsume(Token::IDENTIFIER)) {
                    m_messages.push_back(ParserMessasge{
                        .token = current(),
                        .message = "expected number or identifier inside array access '[]' in variable assignment!"
                    });
                    return std::nullopt;
                }
                indexNode = parseExpression(false);
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
                return varAccess;
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
                return std::make_unique<ast::ArrayAssignment>(std::move(nameToken), std::move(varAccess.value()),
                                                              std::move(value.value()),
                                                              std::move(indexNode.value()));
            }
            if (auto fieldAccess = dynamic_cast<ast::FieldAccess *>(varAccess.value().get())) {
                return std::make_unique<ast::FieldAssignment>(std::move(varAccess.value()->expressionToken()),
                                                              std::move(varAccess.value()),
                                                              std::move(value.value()));
            }
            return std::make_unique<ast::VariableAssignment>(std::move(varAccess.value()->expressionToken()),
                                                             std::move(value.value()));
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
                    .message = "expected '=' after variable name in field assignment!"
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
            auto typeModifier = ast::TypeModifier::NONE;
            if (tryConsume(Token::MUL)) {
                typeModifier = ast::TypeModifier::POINTER;
            } else if (tryConsume(Token::AND)) {
                typeModifier = ast::TypeModifier::REFERENCE;
            }
            auto secondModifier = ast::TypeModifier::NONE;
            if (tryConsume(Token::MUL)) {
                secondModifier = ast::TypeModifier::POINTER;
            }

            std::vector<Token> namespaceElements;
            while (canConsume(Token::IDENTIFIER) && canConsume(Token::NS_SEPARATOR, 1)) {
                namespaceElements.push_back(current());
                consume(Token::IDENTIFIER);
                consume(Token::NS_SEPARATOR);
            }
            auto &typeToken = current();

            if (tryConsume(Token::IDENTIFIER)) {
                std::optional<Token> genericParam = std::nullopt;
                if (tryConsume(Token::LESS)) {
                    genericParam = current();
                    consume(Token::IDENTIFIER);


                    if (!canConsume(Token::GREATER)) {
                        m_messages.push_back(ParserMessasge{
                            .token = current(),
                            .message = "expected '>' to close generic struct declaration!"
                        });
                        return std::nullopt;
                    }
                    consume(Token::GREATER);
                }
                if (secondModifier != ast::TypeModifier::NONE) {
                    return std::make_unique<ast::PointerRawType>(typeToken, namespaceElements, typeModifier,
                                                                 std::make_unique<ast::RawType>(
                                                                     typeToken, namespaceElements, secondModifier,
                                                                     genericParam));
                }

                return std::make_unique<ast::RawType>(typeToken, namespaceElements, typeModifier, genericParam);
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
                const auto &sizeToken = current();
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

                return std::make_unique<ast::ArrayRawType>(typeToken, namespaceElements, typeModifier,
                                                           std::make_unique<ast::RawType>(
                                                               typeToken, namespaceElements, ast::TypeModifier::NONE,
                                                               std::nullopt),
                                                           size);
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
            auto range = parseExpression(false);
            if (!range) {
                m_messages.push_back(ParserMessasge{
                    .token = current(),
                    .message = "expected start expression after 'in' in 'for' loop!"
                });
                return std::nullopt;
            }

            auto block = parseBlock();
            return std::make_unique<ast::ForLoop>(forToken, std::move(iteratorToken), std::move(range.value()),
                                                  isConstant, std::move(block));
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

        std::optional<ast::MatchCase> parseMatch() {
            auto token = tryParseToken();


            if (!token) {
                return std::nullopt;
            }
            std::vector<std::unique_ptr<ast::ASTNode> > matchKeys;
            while (tryConsume(Token::PIPE)) {
                auto nextKey = tryParseToken();
                if (!nextKey) {
                    m_messages.push_back(ParserMessasge{
                        .token = current(),
                        .message = "expected match key after '|' in a match expression!"
                    });
                    return std::nullopt;
                }
                matchKeys.push_back(std::move(nextKey.value()));
            }
            consume(Token::EQUAL);
            consume(Token::GREATER);
            auto expression = parseExpression(false);
            if (!expression) {
                m_messages.push_back(ParserMessasge{
                    .token = current(),
                    .message = "Missing expression after '=>' in a match expression!"
                });
                return std::nullopt;
            }
            consume(Token::COMMA);
            matchKeys.push_back(std::move(token.value()));
            return ast::MatchCase{
                .matchKeys = std::move(matchKeys),
                .expression = std::move(expression.value())
            };
        }

        std::optional<std::unique_ptr<ast::ASTNode> > parseMatchStatement() {
            Token name = current();
            if (!canConsumeKeyWord("match")) {
                return std::nullopt;
            }
            consumeKeyWord("match");
            auto identifier = tryParseToken();
            consume(Token::Type::OPEN_BRACE);
            std::vector<ast::MatchCase> matchCases;
            while (true) {
                auto parsedMatch = parseMatch();
                if (!parsedMatch) {
                    break;
                }
                matchCases.emplace_back(std::move(parsedMatch.value()));
            }
            consume(Token::Type::CLOSE_BRACE);
            consume(Token::SEMICOLON);
            return std::make_unique<ast::MatchExpression>(name, std::move(identifier.value()), std::move(matchCases));
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
                } else if (auto methodCall = parseMethodCall()) {
                    nodes.push_back(std::move(methodCall.value()));
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
                } else if (auto matchStatement = parseMatchStatement()) {
                    nodes.push_back(std::move(matchStatement.value()));
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
            const auto isConstant = !tryConsumeKeyWord("mut");
            auto rawType = parseRawType();
            if (!rawType) {
                m_messages.push_back(ParserMessasge{
                    .token = current(),
                    .message = "expected type after ':' in function argument!"
                });
                return std::nullopt;
            }
            return ast::FunctionArgument(nameToken, std::move(rawType.value()), isConstant);
        }

        std::optional<std::unique_ptr<ast::RawType> > parseExternType() {
            if (canConsumeKeyWord("extern") && canConsumeKeyWord("type", 1)) {
                consumeKeyWord("extern");
                consumeKeyWord("type");

                auto type = parseRawType();
                consume(Token::SEMICOLON);
                return type;
            }
            return std::nullopt;
        }

        std::optional<std::unique_ptr<ast::FunctionDefinitionBase> > parseExternFunctionDefinition() {
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

        std::optional<std::unique_ptr<ast::FunctionDefinitionBase> > parseFunctionDefinition() {
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

            std::optional<Token> genericParam = std::nullopt;
            if (tryConsume(Token::LESS)) {
                genericParam = current();
                consume(Token::IDENTIFIER);


                if (!canConsume(Token::GREATER)) {
                    m_messages.push_back(ParserMessasge{
                        .token = current(),
                        .message = "expected '>' to close generic struct declaration!"
                    });
                    return std::nullopt;
                }
                consume(Token::GREATER);
            }

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
                                                             std::move(block)
                                                             , std::move(genericParam));
        }

        bool isFunctionCall() const {
            int lookaheadIndex = 0;
            if (!canConsume(Token::IDENTIFIER))
                return false;
            lookaheadIndex++;
            while (canConsume(Token::NS_SEPARATOR, lookaheadIndex)) {
                lookaheadIndex++;
                if (!canConsume(Token::IDENTIFIER, lookaheadIndex)) {
                    return false;
                }
                lookaheadIndex++;
            }
            if (canConsume(Token::LESS, lookaheadIndex)) {
                lookaheadIndex++;
                if (!canConsume(Token::IDENTIFIER, lookaheadIndex)) {
                    return false;
                }
                lookaheadIndex++;
                if (!canConsume(Token::GREATER, lookaheadIndex)) {
                    return false;
                }
                lookaheadIndex++;
            }

            if (!canConsume(Token::LEFT_CURLY, lookaheadIndex)) {
                return false;
            }
            return true;
        }

        bool isMethodCall() {
            int lookaheadIndex = 0;
            if (!canConsume(Token::IDENTIFIER))
                return false;
            lookaheadIndex++;
            if (!canConsume(Token::DOT, lookaheadIndex))
                return false;
            lookaheadIndex++;
            if (!canConsume(Token::IDENTIFIER, lookaheadIndex))
                return false;
            lookaheadIndex++;
            if (!canConsume(Token::LEFT_CURLY, lookaheadIndex)) {
                return false;
            }
            return true;
        }

        std::optional<std::unique_ptr<ast::ASTNode> > parseMethodCall() {
            if (!isMethodCall()) {
                return std::nullopt;
            }
            return parseMemberAccess(parseVariableAccess());
        }

        std::optional<std::unique_ptr<ast::FunctionCallNode> > parseFunctionCall() {
            if (!isFunctionCall()) {
                return std::nullopt;
            }
            auto nameToken = current();
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

            std::optional<Token> genericParam = std::nullopt;
            if (tryConsume(Token::LESS)) {
                genericParam = current();
                consume(Token::IDENTIFIER);


                if (!canConsume(Token::GREATER)) {
                    m_messages.push_back(ParserMessasge{
                        .token = current(),
                        .message = "expected '>' to close generic struct declaration!"
                    });
                    return std::nullopt;
                }
                consume(Token::GREATER);
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
            return std::make_unique<ast::FunctionCallNode>(nameToken, namespacePrefix, std::move(args), genericParam);
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
            std::optional<Token> aliasName = std::nullopt;
            if (tryConsumeKeyWord("as")) {
                if (!canConsume(Token::IDENTIFIER)) {
                    m_messages.push_back(ParserMessasge{
                        .token = current(),
                        .message = "expected alias name after 'as' in use module statement!"
                    });
                    return std::nullopt;
                }
                aliasName = current();
                consume(Token::IDENTIFIER);
            }
            consume(Token::SEMICOLON);
            return std::make_unique<ast::UseModule>(modulePath, aliasName);
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
            std::optional<Token> genericParam = std::nullopt;
            if (tryConsume(Token::LESS)) {
                genericParam = current();
                consume(Token::IDENTIFIER);


                if (!canConsume(Token::GREATER)) {
                    m_messages.push_back(ParserMessasge{
                        .token = current(),
                        .message = "expected '>' to close generic struct declaration!"
                    });
                    return std::nullopt;
                }
                consume(Token::GREATER);
            }
            consume(Token::OPEN_BRACE);
            std::vector<ast::StructField> fields;
            std::vector<std::unique_ptr<ast::FunctionDefinitionBase> > methods;

            while (!canConsume(Token::CLOSE_BRACE) && hasNext()) {
                if (auto method = parseFunctionDefinition()) {
                    methods.push_back(std::move(method.value()));
                    continue;
                }
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
            return std::make_unique<ast::StructDeclaration>(std::move(structName), std::move(fields),
                                                            std::move(methods), std::move(genericParam));
        }

        std::optional<std::unique_ptr<ast::ASTNode> > parseEnumDeclaration() {
            if (!canConsumeKeyWord("enum")) {
                return std::nullopt;
            }
            consumeKeyWord("enum");
            if (!canConsume(Token::IDENTIFIER)) {
                m_messages.push_back(ParserMessasge{
                    .token = current(),
                    .message = "expected enum name after 'enum'!"
                });
                return std::nullopt;
            }
            Token enumName = current();
            consume(Token::IDENTIFIER);
            consume(Token::OPEN_BRACE);
            std::vector<ast::EnumVariant> variants;
            while (!canConsume(Token::CLOSE_BRACE) && hasNext()) {
                if (!canConsume(Token::IDENTIFIER)) {
                    m_messages.push_back(ParserMessasge{
                        .token = current(),
                        .message = "expected variant name in enum declaration!"
                    });
                }
                auto variantName = current();
                consume(Token::IDENTIFIER);
                std::optional<std::unique_ptr<ast::ASTNode> > value = std::nullopt;
                if (tryConsume(Token::EQUAL)) {
                    if (!canConsume(Token::NUMBER)) {
                        m_messages.push_back(ParserMessasge{
                            .token = current(),
                            .message = "expected number after '=' in enum variant declaration!"
                        });
                        return std::nullopt;
                    }
                    // Currently we do not use the value, but we can extend this later
                    value = parseNumber();
                }
                variants.push_back(ast::EnumVariant{.name = variantName, .value = std::move(value)});
                if (!tryConsume(Token::COMMA)) {
                    break;
                }
            }
            consume(Token::CLOSE_BRACE);
            consume(Token::SEMICOLON);
            return std::make_unique<ast::EnumDeclaration>(std::move(enumName), std::move(variants));
        }

        ParseResult parse() {
            auto module = std::make_shared<parser::Module>();
            while (!canConsume(Token::END_OF_FILE) && hasNext()) {
                if (auto functionDef = parseFunctionDefinition()) {
                    module->functions.push_back(std::move(functionDef.value()));
                } else if (auto externType = parseExternType()) {
                    module->externTypes.push_back(std::move(externType.value()));
                } else if (auto externFnDefintion = parseExternFunctionDefinition()) {
                    module->functions.push_back(std::move(externFnDefintion.value()));
                } else if (auto useModule = parseUseModule()) {
                    module->useModuleNodes.push_back(std::move(useModule.value()));
                } else if (auto structDecl = parseStructDeclaration()) {
                    module->nodes.push_back(std::move(structDecl.value()));
                } else if (auto enumDecl = parseEnumDeclaration()) {
                    module->nodes.push_back(std::move(enumDecl.value()));
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
                .module = std::move(module),
                .messages = m_messages
            };
        }

    private:
        Token next() {
            if (hasNext())
                ++m_current;
            return current();
        }

        Token &current() {
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

        [[nodiscard]] bool canConsumeKeyWord(const std::string &keyword, const size_t next) const {
            return canConsume(Token::Type::KEYWORD, next) && m_tokens[m_current + next].lexical() == keyword;
        }

        [[nodiscard]] bool canConsumeKeyWord(const std::string &keyword) const {
            return canConsumeKeyWord(keyword, 0);
        }
    };

    std::optional<std::pair<Module *, ast::FunctionDefinitionBase *> > Module::findFunctionsByName(
        const std::string &path,
        const std::string &name) const {
        std::optional<std::string> resolvedAlias = this->aliasName.has_value()
                                                       ? std::make_optional(this->aliasName.value() + "::")
                                                       : std::nullopt;
        for (const auto &f: functions) {
            if (f->functionName() == name and (
                    f->modulePathName() == path or f->modulePathName() == modulePathName()
                    or (resolvedAlias and path == resolvedAlias.value())
                    or (path.empty() and !resolvedAlias)
                )) {
                return std::make_pair(const_cast<Module *>(this), f.get());
            }
        }

        for (const auto &m: modules) {
            std::optional<std::string> aliasName2 = m->aliasName.has_value()
                                                        ? std::make_optional(m->aliasName.value() + "::")
                                                        : std::nullopt;
            if (m->modulePathName() == path or (aliasName2 and path == aliasName2.value())
                or (path.empty() and !aliasName2)
            ) {
                for (const auto &node: m->functions) {
                    if (node->functionName() == name) {
                        return std::make_pair(const_cast<Module *>(m.get()), node.get());
                    }
                }
            }
        }

        return std::nullopt;
    }

    std::optional<std::pair<ast::ASTNode *, ast::ASTNode *> > Module::getNodeByToken(
        const Token &token) const {
        for (auto &node: nodes) {
            if (node->expressionToken() == token) {
                return std::make_pair(nullptr, node.get());
            }
        }
        for (auto &func: functions) {
            if (func->expressionToken() == token) {
                return std::make_pair(nullptr, func.get());
            }
            if (auto funcDef = dynamic_cast<ast::FunctionDefinition *>(func.get())) {
                for (auto &node: funcDef->statements()) {
                    if (const auto result = node->getNodeByToken(token)) {
                        return std::make_pair(func.get(), result.value());
                    }
                }
            }
        }
        return std::nullopt;
    }

    ParseResult parse_tokens(const std::vector<Token> &tokens) {
        Parser parser(tokens);
        return parser.parse();
    }
}
