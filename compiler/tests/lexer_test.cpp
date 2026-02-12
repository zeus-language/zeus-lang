#include <algorithm>

#include "lexer/Lexer.h"
#include <gtest/gtest.h>
#include <magic_enum/magic_enum.hpp>
#include <string>
using namespace std::literals;


void PrintTo(const Token::Type e, std::ostream *os) { *os << magic_enum::enum_name(e); }

// Helper function to verify token position and content
void VerifyTokenPosition(const Token &token, const std::string &source, size_t expectedStart, int row, int col,
                         size_t expectedLength, const std::string &expectedText) {
    EXPECT_EQ(token.source_location.byte_offset, expectedStart)
        << "Token byte offset mismatch for: " << expectedText;
    EXPECT_EQ(token.source_location.num_bytes, expectedLength)
        << "Token byte length mismatch for: " << expectedText;
    EXPECT_EQ(token.lexical(), expectedText) << "Token text mismatch";
    EXPECT_EQ(token.source_location.text(), source.substr(expectedStart, expectedLength))
        << "Token text does not match source at given position";
    EXPECT_EQ(token.source_location.row, row) << "Token row mismatch for: " << expectedText;
    EXPECT_EQ(token.source_location.col, col) << "Token column mismatch for: " << expectedText;
}

void VerifyTokenPosition(const Token &token, const std::string &source, size_t expectedStart,
                         size_t expectedLength, const std::string &expectedText) {
    EXPECT_EQ(token.source_location.byte_offset, expectedStart)
        << "Token byte offset mismatch for: " << expectedText;
    EXPECT_EQ(token.source_location.num_bytes, expectedLength)
        << "Token byte length mismatch for: " << expectedText;
    EXPECT_EQ(token.lexical(), expectedText) << "Token text mismatch";
    EXPECT_EQ(token.source_location.text(), source.substr(expectedStart, expectedLength))
        << "Token text does not match source at given position";
}

TEST(LexerTest, LexIncomplete) {
    std::string source = "ex";
    auto tokens = lexer::lex_file("incomplete.zs", source);

    ASSERT_FALSE(tokens.empty());
    EXPECT_EQ(tokens.size(), 2);

    EXPECT_EQ(tokens[0].type, Token::Type::IDENTIFIER);
    VerifyTokenPosition(tokens[0], source, 0, 2, "ex");

    EXPECT_EQ(tokens[1].type, Token::Type::END_OF_FILE);
}

TEST(LexerTest, LexIdentifiers) {
    std::string source = "hello world test";
    auto tokens = lexer::lex_file("test.zs", source);

    ASSERT_FALSE(tokens.empty());
    EXPECT_EQ(tokens.size(), 4); // 3 identifiers + EOF

    EXPECT_EQ(tokens[0].type, Token::Type::IDENTIFIER);
    VerifyTokenPosition(tokens[0], source, 0, 5, "hello");

    EXPECT_EQ(tokens[1].type, Token::Type::IDENTIFIER);
    VerifyTokenPosition(tokens[1], source, 6, 5, "world");

    EXPECT_EQ(tokens[2].type, Token::Type::IDENTIFIER);
    VerifyTokenPosition(tokens[2], source, 12, 4, "test");

    EXPECT_EQ(tokens[3].type, Token::Type::END_OF_FILE);
}

TEST(LexerTest, LexNumbers) {
    std::string source = "42 3.14 -100 -5.5";
    auto tokens = lexer::lex_file("test.zs", source);

    ASSERT_FALSE(tokens.empty());
    // 4 numbers + EOF
    EXPECT_EQ(tokens.size(), 5);

    EXPECT_EQ(tokens[0].type, Token::Type::NUMBER);
    VerifyTokenPosition(tokens[0], source, 0, 2, "42");

    EXPECT_EQ(tokens[1].type, Token::Type::NUMBER);
    VerifyTokenPosition(tokens[1], source, 3, 4, "3.14");

    EXPECT_EQ(tokens[2].type, Token::Type::NUMBER);
    VerifyTokenPosition(tokens[2], source, 8, 4, "-100");

    EXPECT_EQ(tokens[3].type, Token::Type::NUMBER);
    VerifyTokenPosition(tokens[3], source, 13, 4, "-5.5");

    EXPECT_EQ(tokens[4].type, Token::Type::END_OF_FILE);
}

TEST(LexerTest, LexStrings) {
    std::string source = R"(hello "world" test)";
    auto tokens = lexer::lex_file("test.zs", source);

    ASSERT_FALSE(tokens.empty());
    // identifier + string + identifier + EOF
    EXPECT_EQ(tokens.size(), 4);

    EXPECT_EQ(tokens[0].type, Token::Type::IDENTIFIER);
    VerifyTokenPosition(tokens[0], source, 0, 5, "hello");

    EXPECT_EQ(tokens[1].type, Token::Type::STRING);
    VerifyTokenPosition(tokens[1], source, 6, 7, "\"world\"");

    EXPECT_EQ(tokens[2].type, Token::Type::IDENTIFIER);
    VerifyTokenPosition(tokens[2], source, 14, 4, "test");

    EXPECT_EQ(tokens[3].type, Token::Type::END_OF_FILE);
}

TEST(LexerTest, LexKeywords) {
    std::string source = "fn let return";
    auto tokens = lexer::lex_file("test.zs", source);

    ASSERT_FALSE(tokens.empty());
    // 3 keywords + EOF
    EXPECT_EQ(tokens.size(), 4);

    EXPECT_EQ(tokens[0].type, Token::Type::KEYWORD);
    VerifyTokenPosition(tokens[0], source, 0, 2, "fn");

    EXPECT_EQ(tokens[1].type, Token::Type::KEYWORD);
    VerifyTokenPosition(tokens[1], source, 3, 3, "let");

    EXPECT_EQ(tokens[2].type, Token::Type::KEYWORD);
    VerifyTokenPosition(tokens[2], source, 7, 6, "return");

    EXPECT_EQ(tokens[3].type, Token::Type::END_OF_FILE);
}

TEST(LexerTest, LexOperators) {
    std::string source = "+ - * / % = < > , ; : . ! @ & | ^";
    auto tokens = lexer::lex_file("test.zs", source);

    ASSERT_FALSE(tokens.empty());
    // All operators + EOF
    size_t idx = 0;
    size_t expectedSize = 17 + 1; // 19 operators + EOF
    EXPECT_EQ(tokens.size(), expectedSize);

    std::vector<std::pair<Token::Type, std::string> > expected = {
        {Token::Type::PLUS, "+"}, {Token::Type::MINUS, "-"}, {Token::Type::MUL, "*"},
        {Token::Type::DIV, "/"}, {Token::Type::PERCENT, "%"}, {Token::Type::EQUAL, "="},
        {Token::Type::LESS, "<"}, {Token::Type::GREATER, ">"}, {Token::Type::COMMA, ","},
        {Token::Type::SEMICOLON, ";"}, {Token::Type::COLON, ":"}, {Token::Type::DOT, "."},
        {Token::Type::BANG, "!"}, {Token::Type::AT, "@"}, {Token::Type::AND, "&"},
        {Token::Type::PIPE, "|"}, {Token::Type::CARET, "^"}
    };

    size_t pos = 0;
    for (const auto &[type, text]: expected) {
        EXPECT_EQ(tokens[idx].type, type) << "Token type mismatch at index " << idx;
        // Find position in source
        size_t foundPos = source.find(text, pos);
        ASSERT_NE(foundPos, std::string::npos) << "Cannot find '" << text << "' in source";
        VerifyTokenPosition(tokens[idx], source, foundPos, 1, text);
        pos = foundPos + 1;
        idx++;
    }

    EXPECT_EQ(tokens.back().type, Token::Type::END_OF_FILE);
}

TEST(LexerTest, LexBraces) {
    std::string source = "{ } ( ) [ ]";
    auto tokens = lexer::lex_file("test.zs", source);

    ASSERT_FALSE(tokens.empty());
    // 6 brace tokens + EOF
    EXPECT_EQ(tokens.size(), 7);

    size_t pos = 0;
    std::vector<std::pair<Token::Type, std::string> > expected = {
        {Token::Type::OPEN_BRACE, "{"}, {Token::Type::CLOSE_BRACE, "}"},
        {Token::Type::LEFT_CURLY, "("}, {Token::Type::RIGHT_CURLY, ")"},
        {Token::Type::LEFT_SQUAR, "["}, {Token::Type::RIGHT_SQUAR, "]"}
    };

    for (size_t i = 0; i < expected.size(); i++) {
        EXPECT_EQ(tokens[i].type, expected[i].first);
        size_t foundPos = source.find(expected[i].second, pos);
        ASSERT_NE(foundPos, std::string::npos);
        VerifyTokenPosition(tokens[i], source, foundPos, 1, expected[i].second);
        pos = foundPos + 1;
    }

    EXPECT_EQ(tokens.back().type, Token::Type::END_OF_FILE);
}

TEST(LexerTest, LexComplexExpression) {
    std::string source = "let x = 42 + y;";
    auto tokens = lexer::lex_file("test.zs", source);

    ASSERT_FALSE(tokens.empty());
    // let, x, =, 42, +, y, ; + EOF
    EXPECT_EQ(tokens.size(), 8);

    EXPECT_EQ(tokens[0].type, Token::Type::KEYWORD);
    VerifyTokenPosition(tokens[0], source, 0, 3, "let");

    EXPECT_EQ(tokens[1].type, Token::Type::IDENTIFIER);
    VerifyTokenPosition(tokens[1], source, 4, 1, "x");

    EXPECT_EQ(tokens[2].type, Token::Type::EQUAL);
    VerifyTokenPosition(tokens[2], source, 6, 1, "=");

    EXPECT_EQ(tokens[3].type, Token::Type::NUMBER);
    VerifyTokenPosition(tokens[3], source, 8, 2, "42");

    EXPECT_EQ(tokens[4].type, Token::Type::PLUS);
    VerifyTokenPosition(tokens[4], source, 11, 1, "+");

    EXPECT_EQ(tokens[5].type, Token::Type::IDENTIFIER);
    VerifyTokenPosition(tokens[5], source, 13, 1, "y");

    EXPECT_EQ(tokens[6].type, Token::Type::SEMICOLON);
    VerifyTokenPosition(tokens[6], source, 14, 1, ";");

    EXPECT_EQ(tokens[7].type, Token::Type::END_OF_FILE);
}

TEST(LexerTest, LexRangeOperator) {
    std::string source = "1..10";
    auto tokens = lexer::lex_file("test.zs", source);

    ASSERT_FALSE(tokens.empty());
    // 1, .., 10 + EOF
    EXPECT_EQ(tokens.size(), 4);

    EXPECT_EQ(tokens[0].type, Token::Type::NUMBER);
    VerifyTokenPosition(tokens[0], source, 0, 1, "1");

    EXPECT_EQ(tokens[1].type, Token::Type::RANGE);
    VerifyTokenPosition(tokens[1], source, 1, 2, "..");

    EXPECT_EQ(tokens[2].type, Token::Type::NUMBER);
    VerifyTokenPosition(tokens[2], source, 3, 2, "10");

    EXPECT_EQ(tokens[3].type, Token::Type::END_OF_FILE);
}

TEST(LexerTest, LexNamespaceSeparator) {
    std::string source = "std::vector";
    auto tokens = lexer::lex_file("test.zs", source);

    ASSERT_FALSE(tokens.empty());
    // std, ::, vector + EOF
    EXPECT_EQ(tokens.size(), 4);

    EXPECT_EQ(tokens[0].type, Token::Type::IDENTIFIER);
    VerifyTokenPosition(tokens[0], source, 0, 3, "std");

    EXPECT_EQ(tokens[1].type, Token::Type::NS_SEPARATOR);
    VerifyTokenPosition(tokens[1], source, 3, 2, "::");

    EXPECT_EQ(tokens[2].type, Token::Type::IDENTIFIER);
    VerifyTokenPosition(tokens[2], source, 5, 6, "vector");

    EXPECT_EQ(tokens[3].type, Token::Type::END_OF_FILE);
}

TEST(LexerTest, LexLineComment) {
    std::string source = "let x = 5; // comment";
    auto tokens = lexer::lex_file("test.zs", source, false); // skipComments = false

    ASSERT_FALSE(tokens.empty());
    // let, x, =, 5, ;, comment + EOF
    auto commentToken = std::ranges::find_if(tokens,
                                             [](const Token &t) { return t.type == Token::Type::LINE_COMMENT; });
    ASSERT_NE(commentToken, tokens.end());
    VerifyTokenPosition(*commentToken, source, 11, 10, "// comment");
}

TEST(LexerTest, LexBlockComment) {
    std::string source = "let /* comment */ x = 5;";
    auto tokens = lexer::lex_file("test.zs", source, false); // skipComments = false

    ASSERT_FALSE(tokens.empty());
    auto commentToken = std::find_if(tokens.begin(), tokens.end(),
                                     [](const Token &t) { return t.type == Token::Type::BLOCK_COMMENT; });
    ASSERT_NE(commentToken, tokens.end());
    VerifyTokenPosition(*commentToken, source, 4, 13, "/* comment */");
}

TEST(LexerTest, LexAnnotation) {
    std::string source = "@deprecated fn foo() {}";
    auto tokens = lexer::lex_file("test.zs", source);

    ASSERT_FALSE(tokens.empty());
    EXPECT_EQ(tokens[0].type, Token::Type::ANNOTATION);
    VerifyTokenPosition(tokens[0], source, 0, 11, "@deprecated");
}

TEST(LexerTest, LexMultilineWithRowCol) {
    std::string source = "let x\nlet y";
    auto tokens = lexer::lex_file("test.zs", source);

    ASSERT_FALSE(tokens.empty());
    // let, x, let, y + EOF
    EXPECT_GE(tokens.size(), 4);

    // Check first line
    EXPECT_EQ(tokens[0].type, Token::Type::KEYWORD);
    EXPECT_EQ(tokens[0].source_location.row, 1);
    EXPECT_EQ(tokens[0].source_location.col, 1);

    EXPECT_EQ(tokens[1].type, Token::Type::IDENTIFIER);
    EXPECT_EQ(tokens[1].source_location.row, 1);

    // Check second line
    EXPECT_EQ(tokens[2].type, Token::Type::KEYWORD);
    EXPECT_EQ(tokens[2].source_location.row, 2);
    EXPECT_EQ(tokens[2].source_location.col, 1);

    EXPECT_EQ(tokens[3].type, Token::Type::IDENTIFIER);
    EXPECT_EQ(tokens[3].source_location.row, 2);
}

TEST(LexerTest, LexEmptyString) {
    std::string source = "\"\"";
    auto tokens = lexer::lex_file("test.zs", source);

    ASSERT_FALSE(tokens.empty());
    EXPECT_EQ(tokens.size(), 2); // string + EOF

    EXPECT_EQ(tokens[0].type, Token::Type::STRING);
    VerifyTokenPosition(tokens[0], source, 0, 2, "\"\"");
}

TEST(LexerTest, LexMixedContent) {
    std::string source = "fn add(a: i32, b: i32) -> i32 { a + b }";
    auto tokens = lexer::lex_file("test.zs", source);

    ASSERT_FALSE(tokens.empty());

    // Verify first few tokens have correct positions
    EXPECT_EQ(tokens[0].type, Token::Type::KEYWORD);
    VerifyTokenPosition(tokens[0], source, 0, 2, "fn");

    EXPECT_EQ(tokens[1].type, Token::Type::IDENTIFIER);
    VerifyTokenPosition(tokens[1], source, 3, 3, "add");

    EXPECT_EQ(tokens[2].type, Token::Type::LEFT_CURLY);
    VerifyTokenPosition(tokens[2], source, 6, 1, "(");

    // Verify EOF is present
    EXPECT_EQ(tokens.back().type, Token::Type::END_OF_FILE);

    // Verify all tokens can extract their text from source
    for (size_t i = 0; i < tokens.size() - 1; i++) {
        // Skip EOF
        const auto &token = tokens[i];
        std::string extractedText = source.substr(token.source_location.byte_offset,
                                                  token.source_location.num_bytes);
        EXPECT_EQ(extractedText, token.lexical())
            << "Token at index " << i << " text mismatch";
    }
}

//DrawText(\"Hello World\" as *u8, 190, 200, 20, black);
TEST(LexerTest, LexNumberOffsets) {
    std::string source = "DrawText(\"Hello World\" as *u8, 190, 200, 20, black);";
    auto tokens = lexer::lex_file("test.zs", source);
    ASSERT_FALSE(tokens.empty());
    // verify string token
    auto stringToken = std::find_if(tokens.begin(), tokens.end(),
                                    [](const Token &t) {
                                        return t.type == Token::Type::STRING && t.lexical() == "\"Hello World\"";
                                    });
    ASSERT_NE(stringToken, tokens.end());
    VerifyTokenPosition(*stringToken, source, 9, 1, 10, 13, "\"Hello World\"");


    auto asToken = std::find_if(tokens.begin(), tokens.end(),
                                [](const Token &t) {
                                    return t.type == Token::Type::KEYWORD && t.lexical() == "as";
                                });
    ASSERT_NE(asToken, tokens.end());
    VerifyTokenPosition(*asToken, source, 23, 1, 24, 2, "as");

    auto numberToken190 = std::find_if(tokens.begin(), tokens.end(),
                                       [](const Token &t) {
                                           return t.type == Token::Type::NUMBER && t.lexical() == "190";
                                       });
    ASSERT_NE(numberToken190, tokens.end());
    VerifyTokenPosition(*numberToken190, source, 31, 1, 32, 3, "190");
    auto numberToken200 = std::find_if(tokens.begin(), tokens.end(),
                                       [](const Token &t) {
                                           return t.type == Token::Type::NUMBER && t.lexical() == "200";
                                       });
    ASSERT_NE(numberToken200, tokens.end());
    VerifyTokenPosition(*numberToken200, source, 36, 1, 37, 3, "200");
    auto numberToken20 = std::find_if(tokens.begin(), tokens.end(),
                                      [](const Token &t) {
                                          return t.type == Token::Type::NUMBER && t.lexical() == "20";
                                      });
    ASSERT_NE(numberToken20, tokens.end());
    VerifyTokenPosition(*numberToken20, source, 41, 1, 42, 2, "20");
}


TEST(LexerTest, LexFloat) {
    std::string source = "let pi = 3.14159f;";
    auto tokens = lexer::lex_file("test.zs", source);

    ASSERT_FALSE(tokens.empty());
    // let, pi, =, 3.14159, ; + EOF
    EXPECT_EQ(tokens.size(), 6);

    EXPECT_EQ(tokens[0].type, Token::Type::KEYWORD);
    VerifyTokenPosition(tokens[0], source, 0, 3, "let");

    EXPECT_EQ(tokens[1].type, Token::Type::IDENTIFIER);
    VerifyTokenPosition(tokens[1], source, 4, 2, "pi");

    EXPECT_EQ(tokens[2].type, Token::Type::EQUAL);
    VerifyTokenPosition(tokens[2], source, 7, 1, "=");

    EXPECT_EQ(tokens[3].type, Token::Type::FLOAT_NUMBER);
    VerifyTokenPosition(tokens[3], source, 9, 8, "3.14159f");

    EXPECT_EQ(tokens[4].type, Token::Type::SEMICOLON);
    VerifyTokenPosition(tokens[4], source, 17, 1, ";");

    EXPECT_EQ(tokens[5].type, Token::Type::END_OF_FILE);
}

TEST(LexerStringTest, InterpolateString) {
    std::string source = R"(let greeting = "Hello, ${name}!";)";
    auto tokens = lexer::lex_file("test.zs", source);
    //let greeting = "Hello, ${name}!\";
    ASSERT_FALSE(tokens.empty());
    EXPECT_EQ(tokens.size(), 10); // let, greeting, =, "Hello, ", ${, name, }, "!", ; + EOF
    EXPECT_EQ(tokens[0].type, Token::Type::KEYWORD);
    VerifyTokenPosition(tokens[0], source, 0, 3, "let");
    EXPECT_EQ(tokens[1].type, Token::Type::IDENTIFIER);
    VerifyTokenPosition(tokens[1], source, 4, 8, "greeting");
    EXPECT_EQ(tokens[3].type, Token::Type::INTERPOLATED_STRING);
    VerifyTokenPosition(tokens[3], source, 15, 8, "\"Hello, ");
    EXPECT_EQ(tokens[4].type, Token::Type::INTERPOLATION_START);
    VerifyTokenPosition(tokens[4], source, 23, 2, "${");
    EXPECT_EQ(tokens[5].type, Token::Type::IDENTIFIER);
    VerifyTokenPosition(tokens[5], source, 25, 4, "name");
    EXPECT_EQ(tokens[6].type, Token::Type::INTERPOLATION_END);
    VerifyTokenPosition(tokens[6], source, 29, 1, "}");
    EXPECT_EQ(tokens[7].type, Token::Type::INTERPOLATED_STRING);
    VerifyTokenPosition(tokens[7], source, 30, 2, "!\"");
}
