#pragma once
#include <filesystem>
#include <vector>
#include <string>
#include <set>

namespace lsp {
    struct LspOptions {
        std::set<std::filesystem::path> stdlibDirectories;
        std::string compilerPath;
    };

    std::string shiftarg(std::vector<std::string> &args);

    LspOptions parseLspOptions(std::vector<std::string> &argList);
}
