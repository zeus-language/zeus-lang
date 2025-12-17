//
// Created by stefan on 29.08.25.
//

#ifndef ZEUS_LANG_LEXER_H
#define ZEUS_LANG_LEXER_H
#include <memory>
#include <utility>
#include <vector>
#include <string>

struct SourceLocation {
    std::string filename;
    std::shared_ptr<std::string> source;
    std::size_t byte_offset;
    std::size_t num_bytes;
    size_t row{};
    size_t col{};

    [[nodiscard]] std::string text() const { return source->substr(byte_offset, num_bytes); }
    [[nodiscard]] size_t lineStart() const { return source->rfind('\n', byte_offset) + 1; }

    [[nodiscard]] std::string sourceline() const {
        const size_t endPos = source->find('\n', byte_offset);
        const size_t startPos = lineStart();
        return source->substr(startPos, endPos - startPos);
    }

    bool operator==(const SourceLocation &other) const {
        return filename == other.filename && byte_offset == other.byte_offset && num_bytes == other.num_bytes;
    }
};

struct Token {
    enum Type {
        IDENTIFIER,
        NUMBER,
        STRING,
        KEYWORD,
        END_OF_FILE,
        CHAR,
        PLUS,
        MINUS,
        DIV,
        MUL,
        PERCENT,
        AND,
        LEFT_CURLY,
        RIGHT_CURLY,
        LEFT_SQUAR,
        RIGHT_SQUAR,
        EQUAL,
        GREATER,
        LESS,
        COMMA,
        SEMICOLON,
        DOT,
        COLON,
        CARET,
        BANG,
        AT,
        UNKNOWN,
        OPEN_BRACE,
        CLOSE_BRACE,
        RANGE,
        NS_SEPARATOR,
        PIPE,
        LINE_COMMENT,
        BLOCK_COMMENT,
    } type;

    SourceLocation source_location;

    Token(const Type type, SourceLocation source_location) : type(type), source_location(std::move(source_location)) {
    }

    [[nodiscard]] std::string lexical() const {
        return source_location.text();
    }

    bool operator==(const Token &other) const {
        return source_location == other.source_location;
    }
};

namespace lexer {
    std::vector<Token> lex_file(const std::string &file_path, const std::string &source_code, bool skipComments = true);
};


#endif //ZEUS_LANG_LEXER_H
