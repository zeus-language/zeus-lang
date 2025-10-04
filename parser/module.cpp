#include "parser/module.h"
#include <filesystem>
#include <fstream>
#include <optional>

#include "ast/FunctionDefinition.h"
#include "ast/UseModule.h"
#include "parser/Parser.h"

namespace modules {
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

    void useModuleFile(const std::vector<std::filesystem::path> &stdlibDirectories, parser::ParseResult &result,
                       ast::UseModule *useModule) {
        std::string modulePath;
        for (const auto &ns: useModule->modulePath()) {
            modulePath += ns.lexical() + "/";
        }
        if (!modulePath.empty() && modulePath.back() == '/') {
            modulePath.pop_back();
        }
        modulePath += ".zeus";
        bool moduleLoadedSuccessfully = false;
        for (const auto &basePath: stdlibDirectories) {
            auto fullPath = basePath / modulePath;
            if (std::filesystem::exists(fullPath)) {
                auto moduleContent = read_file(fullPath);
                if (!moduleContent) {
                    result.messages.push_back(parser::ParserMessasge{
                        .outputType = parser::OutputType::ERROR,
                        .token = useModule->expressionToken(),
                        .message = "Could not read module file '" + fullPath.string() + "'!"
                    });
                    break;
                }
                moduleLoadedSuccessfully = true;
                auto moduleTokens = lexer::lex_file(fullPath.string(), moduleContent.value());
                auto moduleResult = parser::parse_tokens(moduleTokens);
                for (const auto &message: moduleResult.messages) {
                    result.messages.push_back(message);
                }
                for (auto &token: moduleResult.nodes) {
                    if (auto subUseModule = dynamic_cast<ast::UseModule *>(token.get())) {
                        useModuleFile(stdlibDirectories, moduleResult, subUseModule);
                        moduleResult.nodes.erase(std::ranges::remove(moduleResult.nodes, token).begin(),
                                                 moduleResult.nodes.end());
                    } else if (auto funcDef = dynamic_cast<ast::FunctionDefinition *>(token.get())) {
                        funcDef->setModulePath(useModule->modulePath());
                        // if (funcDef->isPrivate()) {
                        //     result.messages.push_back(parser::ParserMessasge{
                        //         .outputType = parser::OutputType::ERROR,
                        //         .token = funcDef->expressionToken(),
                        //         .message = "Cannot use private function '" + funcDef->functionName() +
                        //                    "' from module '" + fullPath.string() + "'!"
                        //     });
                        // }
                    }
                }

                result.nodes.insert(result.nodes.end(),
                                    std::make_move_iterator(moduleResult.nodes.begin()),
                                    std::make_move_iterator(moduleResult.nodes.end()));


                break;
            }
        }

        if (!moduleLoadedSuccessfully) {
            result.messages.push_back(parser::ParserMessasge{
                .outputType = parser::OutputType::ERROR,
                .token = useModule->expressionToken(),
                .message = "Could not find module file '" + modulePath + "' in any of the stdlib directories!"
            });
        }
    }

    std::vector<std::unique_ptr<ast::ASTNode> > include_modules(
        const std::vector<std::filesystem::path> &stdlibDirectories,
        parser::ParseResult &result) {
        std::vector<std::unique_ptr<ast::ASTNode> > nodes;
        for (auto &token: result.nodes) {
            if (auto useModule = dynamic_cast<ast::UseModule *>(token.get())) {
                parser::ParseResult moduleResult;
                useModuleFile(stdlibDirectories, moduleResult, useModule);

                for (auto &message: moduleResult.messages) {
                    result.messages.push_back(message);
                }

                for (auto &node: moduleResult.nodes) {
                    nodes.push_back(std::move(node));
                }
            } else {
                nodes.push_back(std::move(token));
            }
        }
        return nodes;
    }
}
