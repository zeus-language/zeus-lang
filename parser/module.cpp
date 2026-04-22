#include "parser/module.h"
#include <filesystem>
#include <fstream>
#include <optional>

#include "dbg_assert.h"
#include "ast/ExternFunctionDefinition.h"
#include "ast/FunctionDefinition.h"
#include "ast/UseModule.h"
#include "parser/Parser.h"
#include "types/TypeChecker.h"

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

    void useModuleFile(const std::vector<std::filesystem::path> &stdlibDirectories, ModuleCache &moduleCache,
                       parser::ParseResult &result, const env::Environment &env,
                       ast::UseModule *useModule) {
        std::string modulePath;
        DBG_ASSERT(result.module, "Resulting module must not be null");
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
            if (!std::filesystem::exists(fullPath)) {
                continue;
            }
            if (moduleCache.containsModule(fullPath.string())) {
                const auto module = moduleCache.getModule(fullPath.string());
                module->setModulePath(useModule->modulePath());
                module->aliasName = useModule->aliasName();
                for (const auto &function: module->functions) {
                    function->setModulePath(useModule->modulePath());
                }
                result.module->modules.push_back(module);
                moduleLoadedSuccessfully = true;
            } else if (!result.module->containsSubModule(modulePath)) {
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
                moduleResult.module->setModulePath(useModule->modulePath());
                moduleResult.module->aliasName = useModule->aliasName();
                for (const auto &message: moduleResult.messages) {
                    result.messages.push_back(message);
                }
                types::TypeCheckResult evalResult;
                types::evaluate_macros(moduleResult.module, env, evalResult);
                for (const auto &message: evalResult.messages) {
                    result.messages.push_back(message);
                }

                result.module->modules.push_back(moduleResult.module);


                std::vector<ast::ASTNode *> nodesToDelete;
                for (auto &node: moduleResult.module->useModuleNodes) {
                    useModuleFile(stdlibDirectories, moduleCache, moduleResult, env, node.get());
                }
                for (const auto &function: moduleResult.module->functions) {
                    function->setModulePath(useModule->modulePath());
                }
                if (!moduleResult.hasError()) {
                    moduleResult.module->sourceFilePath = fullPath;
                    moduleCache.addModule(fullPath.string(), moduleResult.module);
                }
            }


            break;
        }


        if (!moduleLoadedSuccessfully) {
            result.messages.push_back(parser::ParserMessasge{
                .outputType = parser::OutputType::ERROR,
                .token = useModule->expressionToken(),
                .message = "Could not find module file '" + modulePath + "' in any of the stdlib directories!"
            });
        }
    }

    std::shared_ptr<parser::Module> ModuleCache::getModule(const std::string &path) const {
        if (entries.contains(path)) {
            return entries.at(path);
        }
        return nullptr;
    }

    void ModuleCache::addModule(const std::string &path, const std::shared_ptr<parser::Module> &module) {
        if (!isEnabled)
            return;
        entries[path] = std::make_shared<parser::Module>(*module);
    }

    std::vector<std::shared_ptr<parser::Module> > ModuleCache::findModulesByPathStart(
        const std::vector<Token> &pathTokens) const {
        std::vector<std::shared_ptr<parser::Module> > result;
        for (const auto &mod: entries | std::views::values) {
            bool matches = true;
            if (pathTokens.size() == 1 && mod->aliasName.has_value()) {
                if (mod->aliasName.value() == pathTokens[0].lexical()) {
                    result.push_back(mod);
                    continue;
                }
            }
            const auto &modulePath = mod->modulePath();
            if (modulePath.size() < pathTokens.size()) {
                continue;
            }
            for (size_t i = 0; i < pathTokens.size(); i++) {
                if (modulePath[i].lexical() != pathTokens[i].lexical()) {
                    matches = false;
                    break;
                }
            }
            if (matches) {
                result.push_back(mod);
            }
        }
        return result;
    }

    std::vector<std::string> ModuleCache::modulePaths() const {
        std::vector<std::string> result;
        for (const auto &path: entries | std::views::keys) {
            result.push_back(path);
        }
        return result;
    }

    void include_modules(
        const std::vector<std::filesystem::path> &stdlibDirectories, ModuleCache &moduleCache,
        const env::Environment &env,
        parser::ParseResult &result) {
        types::TypeCheckResult evalResult;
        types::evaluate_macros(result.module, env, evalResult);
        for (const auto &message: evalResult.messages) {
            result.messages.push_back(message);
        }
        if (!result.module->modulePathName().starts_with("core") and !result.module->modulePathName().
            starts_with("std")) {
            if (!result.module->containsSubModuleUse("std::io")) {
                const SourceLocation location = result.module->modulePath().empty()
                                                    ? SourceLocation{}
                                                    : result.module->modulePath()[0].source_location;
                auto path = std::vector<Token>{
                    Token("std", Token::IDENTIFIER, location), Token("io", Token::IDENTIFIER, location)
                };
                result.module->useModuleNodes.
                        push_back(std::make_unique<ast::UseModule>(std::move(path), std::nullopt));
            }
            if (!result.module->containsSubModuleUse("core::prelude")) {
                const SourceLocation location = result.module->modulePath().empty()
                                                    ? SourceLocation{}
                                                    : result.module->modulePath()[0].source_location;
                auto path = std::vector<Token>{
                    Token("core", Token::IDENTIFIER, location), Token("prelude", Token::IDENTIFIER, location)
                };
                result.module->useModuleNodes.
                        push_back(std::make_unique<ast::UseModule>(std::move(path), std::nullopt));
            }
        }

        for (auto &token: result.module->useModuleNodes) {
            if (const auto useModule = dynamic_cast<ast::UseModule *>(token.get())) {
                parser::ParseResult moduleResult;

                useModuleFile(stdlibDirectories, moduleCache, result, env, useModule);
            }
        }
    }
}
