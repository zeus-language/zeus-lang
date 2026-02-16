#include "Compiler.h"

#include <fstream>
#include <iostream>
#include <optional>
#include <parser/Parser.h>

#include "ast/FunctionDefinition.h"
#include "ast/UseModule.h"
#include "backends/llvm_backend.h"
#include "parser/module.h"
#include "types/TypeChecker.h"

namespace compiler {
    void compile(CompilerOptions options, const std::string &moduleName,
                 std::ostream &errorStream,
                 std::ostream &outputStream,
                 const std::vector<std::unique_ptr<ast::ASTNode> > &nodes,
                 const std::vector<std::shared_ptr<types::VariableType> > &registeredTypes) {
        llvm_backend::generateExecutable(options, moduleName, errorStream, outputStream,
                                         nodes, registeredTypes);
    }

    std::optional<std::string> read_file(const std::filesystem::path &inputPath) {
        std::ifstream file;
        std::istringstream is;

        file.open(inputPath, std::ios::binary);
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

    std::vector<std::unique_ptr<ast::ASTNode> > moveNodesFromResult(const std::shared_ptr<parser::Module> &module,
                                                                    std::vector<std::string> &visitedModules) {
        std::vector<std::unique_ptr<ast::ASTNode> > nodes;
        for (auto &sub: module->modules) {
            if (visitedModules.end() != std::ranges::find(visitedModules,
                                                          sub->modulePathName())) {
                continue;
            }
            visitedModules.push_back(sub->modulePathName());
            for (auto subNodes = moveNodesFromResult(sub, visitedModules); auto &node: subNodes) {
                nodes.push_back(std::move(node));
            }
        }

        for (auto &node: module->nodes) {
            nodes.push_back(std::move(node));
        }

        for (auto &node: module->functions) {
            nodes.push_back(std::move(node));
        }

        return nodes;
    }

    void parse_and_compile(const compiler::CompilerOptions &options
        ,modules::ModuleCache& moduleCache
        , const std::filesystem::path &inputPath,
                           std::ostream &errorStream,
                           std::ostream &outputStream) {
        const auto content = read_file(inputPath);
        const auto tokens = lexer::lex_file(inputPath.string(), content.value());

        auto result = parser::parse_tokens(tokens);

        modules::include_modules(options.stdlibDirectories,moduleCache, result);
        for (const auto &message: result.messages) {
            message.msg(errorStream, options.colorOutput);
        }
        types::TypeCheckResult typeCheckResult;
        types::type_check(result.module, typeCheckResult);
        for (const auto &message: typeCheckResult.messages) {
            message.msg(errorStream, options.colorOutput);
        }
        std::vector<std::string> visitedModules;
        const auto nodes = moveNodesFromResult(result.module, visitedModules);

        if (!result.hasError() && !typeCheckResult.hasError()) {
            compile(options, inputPath.filename().replace_extension().string(), errorStream, outputStream,
                    nodes, typeCheckResult.registeredTypes);
        }
    }
}
