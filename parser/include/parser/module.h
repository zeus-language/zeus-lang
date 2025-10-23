#pragma once
#include <filesystem>

#include "Parser.h"

namespace modules {
    void include_modules(
        const std::vector<std::filesystem::path> &stdlibDirectories, parser::ParseResult &result);
}
