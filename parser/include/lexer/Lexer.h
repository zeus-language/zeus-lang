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
    [[nodiscard]] size_t lineEnd() const { return source->find('\n', byte_offset); }

    [[nodiscard]] std::string sourceline() const {
        const size_t endPos = lineEnd();
        const size_t startPos = lineStart();
        return source->substr(startPos, endPos - startPos);
    }

    bool operator==(const SourceLocation &other) const {
        return filename == other.filename && byte_offset == other.byte_offset && num_bytes == other.num_bytes;
    }
};

struct Token {
private:
    std::string m_lexical;
public:
    enum Type {
        IDENTIFIER,
        NUMBER,
        FLOAT_NUMBER,
        STRING,
        UNCLOSED_STRING,
        RAW_STRING,
        UNCLOSED_RAW_STRING,
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
        ANNOTATION,
        INTERPOLATION_START,
        INTERPOLATION_END,
        INTERPOLATED_STRING,
    } type;

    SourceLocation source_location;

    Token(std::string lexical, const Type type, SourceLocation source_location) :m_lexical(std::move(lexical)), type(type), source_location(std::move(source_location)) {
    }

    Token(const Type type, SourceLocation source_location) : type(type), source_location(std::move(source_location)) {
    }

    [[nodiscard]] std::string lexical() const {
        if (!m_lexical.empty()) {
            return m_lexical;
        }
        return source_location.text();
    }

    bool operator==(const Token &other) const {
        return source_location == other.source_location && m_lexical == other.m_lexical && type == other.type;
    }
};

namespace lexer {
    std::vector<Token> lex_file(const std::string &file_path, const std::string &source_code, bool skipComments = true);

    std::vector<std::string> keywords();
};


#endif //ZEUS_LANG_LEXER_H
