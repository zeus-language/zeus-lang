//
// Created by stefan on 29.08.25.
//

#include "lexer/Lexer.h"
#include <algorithm>

namespace lexer {
    static std::vector<std::string> possible_tokens = {
        "fn", "return", "let", "mut", "if", "else", "true", "false"
    };


    constexpr bool validStartNameChar(const char value) {
        return (value >= 'A' && value <= 'Z') || (value >= 'a' && value <= 'z') || value == '_';
    }

    constexpr bool validNameChar(const char value) {
        return validStartNameChar(value) || (value >= '0' && value <= '9');
    }

    constexpr bool find_fixed_token(const std::string &content, const size_t start, size_t *endPosition) {
        char current = content[start];
        *endPosition = start + 1;
        if (!validStartNameChar(current))
            return false;

        while (validNameChar(current)) {
            *endPosition += 1;
            current = content[*endPosition];
        }

        const auto tmp = std::string_view(content.data() + start, *endPosition - start);
        return std::ranges::any_of(possible_tokens, [tmp](const std::string &token) { return tmp == token; });
    }


    bool find_string(const std::string &content, size_t start, size_t *endPosition) {
        char current = content[start];
        if (current != '"')
            return false;
        *endPosition = start + 1;
        current = content[start + 1];
        while (true) {
            if (current == '"') {
                if (content.size() - 1 > *endPosition + 1 && content[*endPosition + 1] == '"') {
                    *endPosition += 2;
                    current = content[*endPosition];
                } else {
                    break;
                }
            }

            *endPosition += 1;
            current = content[*endPosition];
        }
        return true;
    }

    bool find_token(const std::string &content, const size_t start, size_t *endPosition) {
        char current = content[start];
        *endPosition = start;
        if (!validStartNameChar(current))
            return false;

        while (validNameChar(current)) {
            *endPosition += 1;
            current = content[*endPosition];
        }
        while (!validNameChar(current)) {
            *endPosition -= 1;
            current = content[*endPosition];
        }
        return true;
    }


    bool find_comment(const std::string &content, const size_t start, size_t *endPosition) {
        if (content[start] == '/' && content[start + 1] == '*') {
            char current = content[start];
            char next = content[start + 1];
            *endPosition = start + 1;

            while (current != '*' && next != '/' && current != 0) {
                *endPosition += 1;
                current = content[*endPosition];
                next = content[*endPosition + 1];
            }
            return true;
        }
        if (content[start] == '/' && content[start + 1] == '/') {
            char current = content[start];
            *endPosition = start + 3;

            while (current != '\n' && current != 0) {
                *endPosition += 1;
                current = content[*endPosition];
            }
            *endPosition -= 1;
            return true;
        }

        return false;
    }

    constexpr bool isNumber(const char c) { return (c >= '0' && c <= '9'); }

    constexpr bool isNumberStart(const char c) { return isNumber(c) || c == '-'; }


    bool find_number(const std::string &content, const size_t start, size_t *endPosition) {
        int index = 0;
        char current = content[start];
        if (current == '-' && !isNumberStart(content[start + 1]))
            return false;
        if (!isNumberStart(current))
            return false;
        while (isNumber(current) or (index == 0 and current == '-') or
               (current == '.' and isNumber(content[*endPosition + 1]))) {
            *endPosition += 1;
            current = content[*endPosition];
            index++;
        }
        if (current < '0' || current > '9')
            *endPosition -= 1;
        return true;
    }

    std::vector<Token> lex_file(const std::string &file_path, const std::string &source_code) {
        std::vector<Token> tokens;
        const auto contentPtr = std::make_shared<std::string>(source_code);
        size_t row = 1;
        size_t col = 1;
        size_t start;
        for (start = 0; start < source_code.length(); start++) {
            size_t endPosition = start;
            bool found = find_string(source_code, start, &endPosition);
            if (found) {
                size_t offset = endPosition - start;
                auto string_length = (offset - 1);
                SourceLocation source_location = {
                    .filename = file_path, .source = contentPtr, .byte_offset = start + 1, .num_bytes = string_length,
                    .row = row,
                    .col = col
                };
                tokens.emplace_back(Token::STRING, source_location);

                start = endPosition;
                col += offset + 1;
                continue;
            }
            found = find_comment(source_code, start, &endPosition);
            if (found) {
                // count lines
                for (size_t i = start; i < endPosition; i++) {
                    if (source_code[i] == '\n')
                        row++;
                }
                size_t offset = endPosition - start;
                start = endPosition;
                col += offset + 1;
                continue;
            }
            found = find_fixed_token(source_code, start, &endPosition);
            if (found) {
                const size_t offset = endPosition - start;


                SourceLocation source_location = {
                    .filename = file_path, .source = contentPtr, .byte_offset = start, .num_bytes = offset, .row = row,
                    .col = col
                };
                tokens.emplace_back(Token::KEYWORD, source_location);
                start = endPosition;
                col += offset;
            }

            found = find_number(source_code, start, &endPosition);
            if (found) {
                const size_t offset = endPosition - start + 1;


                SourceLocation source_location = {
                    .filename = file_path, .source = contentPtr, .byte_offset = start, .num_bytes = offset, .row = row,
                    .col = col
                };
                tokens.emplace_back(Token::NUMBER, source_location);
                start = endPosition;
                col += offset;
            }
            found = find_token(source_code, start, &endPosition);
            if (found) {
                const size_t offset = endPosition - start + 1;
                SourceLocation source_location = {
                    .filename = file_path, .source = contentPtr, .byte_offset = start, .num_bytes = offset, .row = row,
                    .col = col
                };
                tokens.emplace_back(Token::IDENTIFIER, source_location);
                start = endPosition;
                col += offset;
                continue;
            } else {
                SourceLocation source_location = {
                    .filename = file_path, .source = contentPtr, .byte_offset = start, .num_bytes = 1, .row = row,
                    .col = col
                };

                switch (source_code[start]) {
                    case '\n':
                        col = 1;
                        row++;
                        //start++;
                        continue;
                    case '+':
                        tokens.emplace_back(Token::PLUS, source_location);
                        break;
                    case '-':
                        tokens.emplace_back(Token::MINUS, source_location);
                        break;
                    case '*':
                        tokens.emplace_back(Token::MUL, source_location);
                        break;
                    case '/':
                        tokens.emplace_back(Token::DIV, source_location);
                        break;
                    case '(':
                        tokens.emplace_back(Token::LEFT_CURLY, source_location);
                        break;
                    case ')':
                        tokens.emplace_back(Token::RIGHT_CURLY, source_location);
                        break;
                    case '[':
                        tokens.emplace_back(Token::LEFT_SQUAR, source_location);
                        break;
                    case ']':
                        tokens.emplace_back(Token::RIGHT_SQUAR, source_location);
                        break;
                    case '=':
                        tokens.emplace_back(Token::EQUAL, source_location);
                        break;
                    case '<':
                        tokens.emplace_back(Token::LESS, source_location);
                        break;
                    case '>':
                        tokens.emplace_back(Token::GREATER, source_location);
                        break;
                    case ',':
                        tokens.emplace_back(Token::COMMA, source_location);
                        break;
                    case ';':
                        tokens.emplace_back(Token::SEMICOLON, source_location);
                        break;
                    case ':':
                        tokens.emplace_back(Token::COLON, source_location);
                        break;
                    case '.':
                        tokens.emplace_back(Token::DOT, source_location);
                        break;
                    case '^':
                        tokens.emplace_back(Token::CARET, source_location);
                        break;
                    case '!':
                        tokens.emplace_back(Token::BANG, source_location);
                        break;
                    case '@':
                        tokens.emplace_back(Token::AT, source_location);
                        break;
                    case '{':
                        tokens.emplace_back(Token::OPEN_BRACE, source_location);
                        break;
                    case '}':
                        tokens.emplace_back(Token::CLOSE_BRACE, source_location);
                        break;
                    default:
                        break;
                }

                col++;
            }
        }
        SourceLocation source_location = {
            .filename = file_path, .source = contentPtr, .byte_offset = start, .num_bytes = 1, .row = row,
            .col = col
        };
        tokens.emplace_back(Token::END_OF_FILE, source_location);
        return tokens;
    }
}
