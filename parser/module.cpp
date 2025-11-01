#include "parser/module.h"
#include <filesystem>
#include <fstream>
#include <optional>

#include "ast/ExternFunctionDefinition.h"
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
        assert(result.module && "Resulting module must not be null");
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
            if (std::filesystem::exists(fullPath) and (
                    !result.module->containsSubModule(modulePath))) {
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
                moduleResult.module->modulePath = useModule->modulePath();
                moduleResult.module->aliasName = useModule->aliasName();
                for (const auto &message: moduleResult.messages) {
                    result.messages.push_back(message);
                }

                if (!result.hasError()) {
                    result.module->modules.push_back(moduleResult.module);

                    std::vector<ast::ASTNode *> nodesToDelete;
                    for (auto &node: moduleResult.module->useModuleNodes) {
                        if (const auto subUseModule = dynamic_cast<ast::UseModule *>(node.get())) {
                            useModuleFile(stdlibDirectories, moduleResult, subUseModule);
                        }
                    }
                    for (auto &function: moduleResult.module->functions) {
                        function->setModulePath(useModule->modulePath());
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

    void include_modules(
        const std::vector<std::filesystem::path> &stdlibDirectories,
        parser::ParseResult &result) {
        for (auto &token: result.module->useModuleNodes) {
            if (const auto useModule = dynamic_cast<ast::UseModule *>(token.get())) {
                parser::ParseResult moduleResult;

                useModuleFile(stdlibDirectories, result, useModule);
            }
        }
    }
}
