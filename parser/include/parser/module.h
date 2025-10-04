#pragma once
#include <filesystem>

#include "Parser.h"

namespace modules {
    std::vector<std::unique_ptr<ast::ASTNode> > include_modules(
        const std::vector<std::filesystem::path> &stdlibDirectories, parser::ParseResult &result);
}
