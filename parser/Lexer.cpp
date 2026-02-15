//
// Created by stefan on 29.08.25.
//

#include "lexer/Lexer.h"
#include <algorithm>

namespace lexer
{
    static std::vector<std::string> possible_tokens = {
            "fn", "return", "let", "mut", "if", "else", "true", "false", "while", "for", "in", "break", "continue",
            "use",
            "or", "and", "as", "struct", "extern", "match", "enum", "null", "type", "not"
    };


    constexpr bool validStartNameChar(const char value)
    {
        return (value >= 'A' && value <= 'Z') || (value >= 'a' && value <= 'z') || value == '_';
    }

    constexpr bool validNameChar(const char value)
    {
        return validStartNameChar(value) || (value >= '0' && value <= '9');
    }

    constexpr bool find_fixed_token(const std::string &content, const size_t start, size_t *endPosition)
    {
        char current = content[start];
        *endPosition = start + 1;
        if (!validStartNameChar(current))
            return false;

        while (validNameChar(current) && *endPosition < content.size())
        {
            *endPosition += 1;
            current = content[*endPosition];
        }

        const auto tmp = std::string_view(content.data() + start, *endPosition - start);
        return std::ranges::any_of(possible_tokens, [tmp](const std::string &token) { return tmp == token; });
    }

    constexpr bool isNumber(const char c) { return (c >= '0' && c <= '9'); }

    constexpr bool isNumberStart(const char c) { return isNumber(c) || c == '-'; }

    class Lexer
    {
        std::vector<Token> tokens;
        std::shared_ptr<std::string> contentPtr;
        size_t row = 1;
        size_t col = 1;
        size_t start = 0;
        std::string file_path;

        bool find_raw_string(const std::string &content, size_t start, size_t *endPosition)
        {
            char current = content[start];
            if (current != 'r' || content[start + 1] != '"')
                return false;
            *endPosition = start + 2;
            current = content[start + 2];
            while (true)
            {
                if (current == '"')
                {
                    *endPosition += 1;

                    break;
                }

                *endPosition += 1;
                current = content[*endPosition];
            }
            return true;
        }

        void addTokenToResult(size_t tempStart, size_t endPosition, Token::Type type)
        {
            size_t offset = endPosition - tempStart;
            auto string_length = (offset);
            SourceLocation source_location = {
                    .filename = file_path, .source = contentPtr, .byte_offset = tempStart, .num_bytes = string_length,
                    .row = row,
                    .col = col
            };
            tokens.emplace_back(type, source_location);


            col += offset;
        }

        bool find_string(const std::string &content, size_t *endPosition)
        {
            char current = content[start];
            if (current != '"')
                return false;
            *endPosition = start + 1;
            current = content[start + 1];
            size_t tempStart = start;
            bool isInterpolation = false;
            while (true)
            {
                if (current == '"')
                {
                    *endPosition += 1;
                    break;
                }
                size_t oldEndPosition = *endPosition;
                if (current == '$' &&  content[*endPosition+1] == '{' && *endPosition + 1 < content.size())
                {
                    isInterpolation = true;
                    *endPosition += 1;
                    current = content[*endPosition];
                    if (current == '{')
                    {
                        *endPosition += 1;
                        current = content[*endPosition];
                        addTokenToResult(tempStart, oldEndPosition, Token::INTERPOLATED_STRING);
                        tempStart = oldEndPosition ;
                        addTokenToResult(tempStart, *endPosition, Token::INTERPOLATION_START);
                        tempStart = *endPosition ;
                        continue;
                    }
                }
                if (isInterpolation && current == '}')
                {
                    *endPosition += 1;
                    addTokenToResult(tempStart, oldEndPosition, Token::IDENTIFIER);
                    tempStart = oldEndPosition;

                    addTokenToResult(tempStart, *endPosition, Token::INTERPOLATION_END);
                    tempStart = *endPosition;
                    current = content[*endPosition];
                    continue;
                    //isInterpolation = false;
                }

                *endPosition += 1;
                current = content[*endPosition];
            }
            if (isInterpolation)
                addTokenToResult(tempStart, *endPosition, Token::INTERPOLATED_STRING);
            else
                addTokenToResult(tempStart, *endPosition, Token::STRING);

            start = *endPosition - 1;
            return true;
        }

        bool find_token(const std::string &content, const size_t start, size_t *endPosition)
        {
            char current = content[start];
            *endPosition = start;
            if (!validStartNameChar(current))
                return false;

            while (validNameChar(current))
            {
                *endPosition += 1;
                current = content[*endPosition];
            }
            while (!validNameChar(current))
            {
                *endPosition -= 1;
                current = content[*endPosition];
            }
            return true;
        }


        bool find_comment(const std::string &content, const size_t start, size_t *endPosition)
        {
            if (content[start] == '/' && content[start + 1] == '*')
            {
                char current = content[start];
                char next = content[start + 1];
                *endPosition = start + 1;

                while (!(current == '*' && next == '/') && current != 0)
                {
                    *endPosition += 1;
                    current = content[*endPosition];
                    next = content[*endPosition + 1];
                }
                *endPosition += 1;
                return true;
            }
            if (content[start] == '/' && content[start + 1] == '/')
            {
                char current = content[start];
                *endPosition = start + 3;

                while (current != '\n' && current != 0)
                {
                    *endPosition += 1;
                    current = content[*endPosition];
                }
                *endPosition -= 1;
                return true;
            }

            return false;
        }


        bool find_number(const std::string &content, const size_t start, size_t *endPosition,
                         Token::Type *numberTokenType)
        {
            *endPosition = start;
            int index = 0;
            char current = content[start];
            if (current == '-' && !isNumberStart(content[start + 1]))
                return false;
            if (!isNumberStart(current))
                return false;
            *endPosition += 1;
            current = content[*endPosition];
            index++;
            while (isNumber(current) or (index == 0 and current == '-') or
                   (current == '.' and isNumber(content[*endPosition + 1])))
            {
                *endPosition += 1;
                current = content[*endPosition];
                index++;
            };
            if (current == 'f' || current == 'F')
            {
                *numberTokenType = Token::FLOAT_NUMBER;
                *endPosition += 1;
            }
            // if (current < '0' || current > '9')
            //     *endPosition -= 1;
            return true;
        }

        bool lex_annotation(const std::string &content, size_t *endPosition) const
        {
            char current = content[start];
            *endPosition = start;
            if (!(current == '@' && validNameChar(content[start + 1])))
                return false;

            do
            {
                *endPosition += 1;
                current = content[*endPosition];
            }
            while (validNameChar(current));

            return true;
        }

    public:
        std::vector<Token> lex_file(const std::string &file_path, const std::string &source_code, bool skipComments)
        {
            contentPtr = std::make_shared<std::string>(source_code);
            this->file_path = file_path;

            for (start = 0; start < source_code.length(); start++)
            {
                size_t endPosition = start;
                bool found = find_raw_string(source_code, start, &endPosition);
                if (found)
                {
                    size_t offset = endPosition - start;
                    auto string_length = (offset);
                    SourceLocation source_location = {
                            .filename = file_path, .source = contentPtr, .byte_offset = start,
                            .num_bytes = string_length,
                            .row = row,
                            .col = col
                    };
                    tokens.emplace_back(Token::RAW_STRING, source_location);

                    start = endPosition - 1;
                    col += offset;
                    continue;
                }
                found = find_string(source_code, &endPosition);
                if (found)
                {
                    continue;
                }
                if (source_code[start] == '\'')
                {
                    SourceLocation source_location = {
                            .filename = file_path, .source = contentPtr, .byte_offset = start, .num_bytes = 3,
                            .row = row,
                            .col = col
                    };
                    tokens.emplace_back(Token::CHAR, source_location);
                    col += 3;
                    start += 2;
                    continue;
                }

                found = find_comment(source_code, start, &endPosition);
                if (found)
                {
                    // count lines

                    size_t offset = endPosition - start + 1;

                    if (!skipComments)
                    {
                        SourceLocation source_location = {
                                .filename = file_path, .source = contentPtr, .byte_offset = start, .num_bytes = offset,
                                .row = row,
                                .col = col
                        };
                        if (source_code[start] == '/' && source_code[start + 1] == '/')
                        {
                            tokens.emplace_back(Token::LINE_COMMENT, source_location);
                        }
                        else
                        {
                            tokens.emplace_back(Token::BLOCK_COMMENT, source_location);
                        }
                    }
                    for (size_t i = start; i < endPosition; i++)
                    {
                        if (source_code[i] == '\n')
                            row++;
                    }
                    start = endPosition;
                    col += offset;
                    continue;
                }
                found = find_fixed_token(source_code, start, &endPosition);
                if (found)
                {
                    const size_t offset = endPosition - start;


                    SourceLocation source_location = {
                            .filename = file_path, .source = contentPtr, .byte_offset = start, .num_bytes = offset,
                            .row = row,
                            .col = col
                    };
                    tokens.emplace_back(Token::KEYWORD, source_location);
                    start = endPosition;
                    col += offset;
                }
                Token::Type numberTokenType = Token::NUMBER;
                found = find_number(source_code, start, &endPosition, &numberTokenType);
                if (found)
                {
                    const size_t offset = endPosition - start;


                    SourceLocation source_location = {
                            .filename = file_path, .source = contentPtr, .byte_offset = start, .num_bytes = offset,
                            .row = row,
                            .col = col
                    };
                    tokens.emplace_back(numberTokenType, source_location);
                    start = endPosition;

                    col += offset;
                }
                found = lex_annotation(source_code, &endPosition);
                if (found)
                {
                    const size_t offset = endPosition - start;
                    SourceLocation source_location = {
                            .filename = file_path, .source = contentPtr, .byte_offset = start, .num_bytes = offset,
                            .row = row,
                            .col = col
                    };
                    tokens.emplace_back(Token::ANNOTATION, source_location);
                    start = endPosition - 1;
                    col += offset;
                    continue;
                }
                found = find_token(source_code, start, &endPosition);
                if (found)
                {
                    const size_t offset = endPosition - start + 1;
                    SourceLocation source_location = {
                            .filename = file_path, .source = contentPtr, .byte_offset = start, .num_bytes = offset,
                            .row = row,
                            .col = col
                    };
                    tokens.emplace_back(Token::IDENTIFIER, source_location);
                    start = endPosition;
                    col += offset;
                    continue;
                }
                if (source_code[start] == '.' && source_code[start + 1] == '.')
                {
                    SourceLocation source_location = {
                            .filename = file_path, .source = contentPtr, .byte_offset = start, .num_bytes = 2,
                            .row = row,
                            .col = col
                    };
                    tokens.emplace_back(Token::RANGE, source_location);
                    start++;
                    col += 2;
                    continue;
                }
                if (source_code[start] == ':' && source_code[start + 1] == ':')
                {
                    SourceLocation source_location = {
                            .filename = file_path, .source = contentPtr, .byte_offset = start, .num_bytes = 2,
                            .row = row,
                            .col = col
                    };
                    tokens.emplace_back(Token::NS_SEPARATOR, source_location);
                    start++;
                    col += 2;
                    continue;
                }
                else
                {
                    SourceLocation source_location = {
                            .filename = file_path, .source = contentPtr, .byte_offset = start, .num_bytes = 1,
                            .row = row,
                            .col = col
                    };

                    switch (source_code[start])
                    {
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
                        case '%':
                            tokens.emplace_back(Token::PERCENT, source_location);
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
                        case '&':
                            tokens.emplace_back(Token::AND, source_location);
                            break;
                        case '|':
                            tokens.emplace_back(Token::PIPE, source_location);
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


    };

    std::vector<Token> lex_file(const std::string &file_path, const std::string &source_code, bool skipComments)
    {
        Lexer lexer;
        return lexer.lex_file(file_path, source_code, skipComments);
    }

    std::vector<std::string> keywords()
    {
        return possible_tokens;
    }
}
