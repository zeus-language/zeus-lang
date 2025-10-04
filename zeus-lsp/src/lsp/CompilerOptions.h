#pragma once
#include <filesystem>
#include <vector>
#include <string>

namespace lsp {
    struct LspOptions {
        std::vector<std::filesystem::path> stdlibDirectories;
        std::string compilerPath;
    };

    std::string shiftarg(std::vector<std::string> &args);

    LspOptions parseLspOptions(std::vector<std::string> &argList);
}
