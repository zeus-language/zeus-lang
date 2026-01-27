#pragma once
#include <filesystem>
#include <map>

#include "Parser.h"

namespace modules {

    class ModuleCache {
    private:
        std::map<std::string, std::shared_ptr<parser::Module>> entries;
        bool isEnabled = true;
    public:
        [[nodiscard]] std::shared_ptr<parser::Module> getModule(const std::string &path) const;
        void addModule(const std::string &path, const std::shared_ptr<parser::Module> &module) ;
        [[nodiscard]] bool containsModule(const std::string &path) const {
            return entries.contains(path);
        }

        [[nodiscard]] std::vector<std::shared_ptr<parser::Module>> findModulesByPathStart(const std::vector<Token> & pathTokens) const;

        explicit ModuleCache(bool enabled) : isEnabled(enabled) {}
    };



    void include_modules(
        const std::vector<std::filesystem::path> &stdlibDirectories,ModuleCache& moduleCache, parser::ParseResult &result);
}
