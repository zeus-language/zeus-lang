#include "Compiler.h"

#include <fstream>
#include <iostream>
#include <optional>
#include <parser/Parser.h>

#include "backends/llvm_backend.h"

namespace compiler {
    void compile(CompilerOptions options, const std::string &moduleName,
                 std::ostream &errorStream,
                 std::ostream &outputStream,
                 const std::vector<std::unique_ptr<ast::ASTNode> > &nodes) {
        llvm_backend::generateExecutable(options, moduleName, errorStream, outputStream,
                                         nodes);
    }

    std::optional<std::string> read_file(const std::filesystem::path &inputPath) {
        std::ifstream file;
        std::istringstream is;

        file.open(inputPath, std::ios::in);
        if (!file.is_open()) {
            return std::nullopt;
        }
        file.seekg(0, std::ios::end);
        std::streamsize size = file.tellg();
        std::string buffer(size, ' ');
        file.seekg(0);
        file.read(&buffer[0], size);
        return buffer;
    }

    void parse_and_compile(const compiler::CompilerOptions &options, const std::filesystem::path &inputPath,
                           std::ostream &errorStream,
                           std::ostream &outputStream) {
        const auto content = read_file(inputPath);
        const auto tokens = lexer::lex_file(inputPath, content.value());
        // for (const auto &token: tokens) {
        //   std::cout << "Token Type: " << std::string(
        //         magic_enum::enum_name(token.type)) << " Lexical: " << token.lexical() << " at Line: " << token.source_location
        //       .row
        //       << ", Column: " << token.source_location.col << std::endl;
        // }
        auto [nodes, messages] = parser::parse_tokens(tokens);
        for (const auto &message: messages) {
            message.msg(std::cerr, true);
        }

        compile(options, inputPath.filename().replace_extension().string(), errorStream, outputStream,
                nodes);
    }
}
