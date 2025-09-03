//
// Created by stefan on 29.08.25.
//

#include  "parser/Parser.h"

#include <cassert>
#include <iomanip>
#include <iostream>

#include "ast/BinaryExpression.h"
#include "ast/FunctionCallNode.h"
#include "ast/FunctionDefinition.h"
#include "ast/LogicalExpression.h"
#include "ast/NumberConstant.h"
#include "ast/ReturnStatement.h"
#include "ast/StringConstant.h"
#include "ast/VariableAccess.h"
#include "ast/VariableAssignment.h"
#include "ast/VariableDeclaration.h"
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
            auto returnValue = tryParseToken();
            return std::make_unique<ast::ReturnStatement>(returnToken, std::move(returnValue));
        }

        std::optional<std::unique_ptr<ast::ASTNode> > tryParseToken() {
            if (auto number = parseNumber()) {
                return std::move(number.value());
            }
            if (auto string = parseString()) {
                return std::move(string.value());
            }
            if (auto functionCall = parseFunctionCall()) {
                return std::move(functionCall.value());
            }
            if (auto varAccess = parseVariableAccess()) {
                return std::move(varAccess.value());
            }
            return std::nullopt;
        }

        std::optional<std::unique_ptr<ast::ASTNode> > parseVariableAccess() {
            if (!canConsume(Token::IDENTIFIER)) {
                return std::nullopt;
            }
            Token nameToken = current();
            consume(Token::IDENTIFIER);
            return std::make_unique<ast::VariableAccess>(std::move(nameToken));
        }


        std::optional<std::unique_ptr<ast::ASTNode> > parseBaseExpression(
            std::optional<std::unique_ptr<ast::ASTNode> > origLhs = std::nullopt, bool includeCompare = true) {
            auto lhs = (origLhs.has_value()) ? std::move(origLhs) : tryParseToken();


            Token operatorToken = current();
            if (lhs) {
                if (tryConsume(Token::PLUS)) {
                    auto rhs = tryParseToken();

                    if (canConsume(Token::MUL) or canConsume(Token::DIV) or canConsume(Token::LEFT_CURLY)) {
                        rhs = parseBaseExpression(std::move(rhs), false);
                    }
                    if (!rhs) {
                        m_messages.push_back(ParserMessasge{
                            .token = current(),
                            .message = "missing right hand side expression after '+' operator",
                        });
                        return lhs;
                    }

                    return parseExpression(
                        std::make_unique<ast::BinaryExpression>(operatorToken, ast::BinaryOperator::ADD,
                                                                std::move(lhs.value()),
                                                                std::move(rhs.value())));
                }
                if (tryConsume(Token::MINUS)) {
                    auto rhs = tryParseToken();
                    if (canConsume(Token::MUL) or canConsume(Token::DIV) or canConsume(Token::LEFT_CURLY)) {
                        rhs = parseBaseExpression(std::move(rhs), false);
                    }

                    return parseExpression(
                        std::make_unique<ast::BinaryExpression>(operatorToken, ast::BinaryOperator::SUB,
                                                                std::move(lhs.value()), std::move(rhs.value())));
                }
                if (tryConsume(Token::MUL)) {
                    auto rhs = tryParseToken();
                    return parseExpression(
                        std::make_unique<ast::BinaryExpression>(operatorToken, ast::BinaryOperator::MUL,
                                                                std::move(lhs.value()), std::move(rhs.value())));
                }
                if (tryConsume(Token::DIV)) {
                    auto rhs = tryParseToken();
                    return parseExpression(
                        std::make_unique<ast::BinaryExpression>(operatorToken, ast::BinaryOperator::DIV,
                                                                std::move(lhs.value()), std::move(rhs.value())));
                }
            }

            if (canConsume(Token::LEFT_CURLY)) {
                consume(Token::LEFT_CURLY);
                auto result = parseExpression(std::nullopt);
                consume(Token::RIGHT_CURLY);
                if (lhs.has_value())
                    if (auto binOp = dynamic_cast<ast::BinaryExpression *>(lhs.value().get())) {
                        result = std::make_unique<ast::BinaryExpression>(binOp->expressionToken(), binOp->binoperator(),
                                                                         binOp->lhs(),
                                                                         std::move(result.value()));
                    }
                return parseExpression(std::move(result));
            }

            if (includeCompare) {
                // if (canConsume(Token::GREATER)) {
                //     consume(Token::GREATER);
                //     auto operatorToken = current();
                //     if (canConsume(Token::EQUAL)) {
                //         consume(Token::EQUAL);
                //         auto rhs = parseBaseExpression();
                //         return std::make_shared<ast::Comparrision>(operatorToken, CMPOperator::GREATER_EQUAL, lhs, rhs);
                //     }
                //     auto rhs = parseBaseExpression();
                //     return parseExpression(
                //         std::make_shared<ComparrisionNode>(
                //             operatorToken, CMPOperator::GREATER, lhs, rhs));
                // }
                //
                // if (canConsume(Token::LESS)) {
                //     consume(Token::LESS);
                //     auto operatorToken = current();
                //     if (canConsume(Token::EQUAL)) {
                //         consume(Token::EQUAL);
                //         auto rhs = parseBaseExpression();
                //         return std::make_shared<ComparrisionNode>(operatorToken, CMPOperator::LESS_EQUAL, lhs, rhs);
                //     } else if (canConsume(Token::GREATER)) {
                //         consume(Token::GREATER);
                //         auto rhs = parseBaseExpression();
                //         return parseExpression(
                //             std::make_shared<ComparrisionNode>(operatorToken, CMPOperator::NOT_EQUALS, lhs,
                //                                                rhs));
                //     }
                //     auto rhs = parseBaseExpression();
                //     return parseExpression(
                //         std::make_shared<ComparrisionNode>(
                //             operatorToken, CMPOperator::LESS, lhs, rhs));
                // }
                //
                // if (canConsume(Token::BANG) && canConsume(Token::EQUAL, 2)) {
                //     consume(Token::BANG);
                //     consume(Token::EQUAL);
                //     auto operatorToken = current();
                //     auto rhs = parseBaseExpression();
                //     return parseExpression(
                //         std::make_shared<ComparrisionNode>(operatorToken, CMPOperator::NOT_EQUALS, lhs, rhs));
                // }
                //
                // if (canConsume(Token::EQUAL)) {
                //     consume(Token::EQUAL);
                //     auto operatorToken = current();
                //     auto rhs = parseBaseExpression();
                //     return parseExpression(
                //         std::make_shared<Comparrision>(
                //             operatorToken, CMPOperator::EQUALS, lhs, rhs));
                // }
            }

            return lhs;
        }

        std::optional<std::unique_ptr<ast::ASTNode> > parseLogicalExpression(
            std::optional<std::unique_ptr<ast::ASTNode> > lhs) {
            if (canConsumeKeyWord("not")) {
                consumeKeyWord("not");
                auto token = current();
                auto rhs = parseExpression();
                return parseExpression(
                    std::make_unique<ast::LogicalExpression>(token, ast::LogicalOperator::NOT,
                                                             std::move(rhs.value())));
            }

            if (!lhs)
                return std::nullopt;

            if (canConsumeKeyWord("or")) {
                consumeKeyWord("or");
                auto token = current();
                auto rhs = parseExpression();
                return parseExpression(
                    std::make_unique<ast::LogicalExpression>(token, ast::LogicalOperator::OR,
                                                             std::move(lhs.value()),
                                                             std::move(rhs.value())));
            }
            if (canConsumeKeyWord("and")) {
                consumeKeyWord("and");
                auto token = current();
                auto rhs = parseExpression();
                return parseExpression(
                    std::make_unique<
                        ast::LogicalExpression>(token, ast::LogicalOperator::AND, std::move(lhs.value()),
                                                std::move(rhs.value())));
            }
            return lhs;
        }

        std::optional<std::unique_ptr<ast::ASTNode> > parseExpression(
            std::optional<std::unique_ptr<ast::ASTNode> > origLhs = std::nullopt) {
            auto lhs = parseLogicalExpression(std::move(origLhs));
            if (!lhs)
                lhs = std::move(origLhs);

            if (!lhs)
                lhs = parseBaseExpression();
            if (!lhs)
                return std::nullopt;

            if (auto rhs = parseLogicalExpression(parseBaseExpression(std::move(lhs))))
                return rhs;

            return lhs;
        }

        std::optional<std::unique_ptr<ast::ASTNode> > parseVariableAssignment() {
            if (!canConsume(Token::IDENTIFIER) || !canConsume(Token::EQUAL, 1)) {
                return std::nullopt;
            }
            Token nameToken = current();
            consume(Token::IDENTIFIER);
            if (!tryConsume(Token::EQUAL)) {
                m_messages.push_back(ParserMessasge{
                    .token = nameToken,
                    .message = "expected '=' after variable name in assignment!"
                });
                return std::nullopt;
            }
            auto value = parseExpression();
            if (!value) {
                m_messages.push_back(ParserMessasge{
                    .token = current(),
                    .message = "expected expression after '=' in variable assignment!"
                });
                return std::nullopt;
            }
            consume(Token::SEMICOLON);
            return std::make_unique<ast::VariableAssignment>(std::move(nameToken), std::move(value.value()));
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
            Token typeToken = current();
            consume(Token::Type::IDENTIFIER);

            std::optional<std::unique_ptr<ast::ASTNode> > value = std::nullopt;
            if (tryConsume(Token::EQUAL)) {
                value = tryParseToken();
                consume(Token::Type::SEMICOLON);
            } else {
                consume(Token::Type::SEMICOLON);
            }

            return std::make_unique<ast::VariableDeclaration>(std::move(nameToken), std::move(typeToken), constant,
                                                              std::move(value));
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
                } else {
                    next();
                    m_messages.push_back(ParserMessasge{
                        .token = current(),
                        .message = "unexpected token found " +
                                   std::string(magic_enum::enum_name(current().type)) + "!"
                    });
                    if (current().type == Token::Type::END_OF_FILE) {
                        break;
                    }
                }
            }
            consume(Token::Type::CLOSE_BRACE);
            return nodes;
        }

        std::unique_ptr<ast::ASTNode> parseFunctionDefinition() {
            consumeKeyWord("fn");
            Token nameToken = current();
            consume(Token::Type::IDENTIFIER);

            consume(Token::Type::LEFT_CURLY);
            // todo parse parameters
            std::vector<ast::FunctionArgument> functionArgs;
            while (!canConsume(Token::Type::RIGHT_CURLY) && hasNext()) {
                next(); //TODO
            }
            consume(Token::Type::RIGHT_CURLY);
            consume(Token::COLON);
            consume(Token::IDENTIFIER);
            auto returnType = current();

            auto block = parseBlock();
            return std::make_unique<ast::FunctionDefinition>(std::move(nameToken), functionArgs, std::move(block));
        }

        std::optional<std::unique_ptr<ast::FunctionCallNode> > parseFunctionCall() {
            if ((!canConsume(Token::IDENTIFIER) || !canConsume(Token::Type::LEFT_CURLY, 1))) {
                return std::nullopt;
            }
            Token nameToken = current();
            consume(Token::Type::IDENTIFIER);
            consume(Token::Type::LEFT_CURLY);
            // todo parse parameters
            std::vector<std::unique_ptr<ast::ASTNode> > args;
            while (!canConsume(Token::Type::RIGHT_CURLY) && hasNext()) {
                if (auto arg = tryParseToken()) {
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
            return std::make_unique<ast::FunctionCallNode>(nameToken, std::move(args));
        }

        ParseResult parse() {
            std::vector<std::unique_ptr<ast::ASTNode> > nodes;
            nodes.push_back(std::move(parseFunctionDefinition()));
            return ParseResult{
                .nodes = std::move(nodes),
                .messages = m_messages
            };
        }

    private:
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

        [[nodiscard]] bool hasNext() const { return m_current < m_tokens.size() - 1; }

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
