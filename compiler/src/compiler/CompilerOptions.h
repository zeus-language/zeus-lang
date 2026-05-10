#pragma once
#include <filesystem>
#include <optional>
#include <vector>

namespace compiler {
    enum class BuildMode {
        Debug,
        Release
    };

    struct CompilerOptions {
        BuildMode buildMode = BuildMode::Debug;

        std::filesystem::path outputDirectory;
        std::vector<std::filesystem::path> stdlibDirectories;
        std::vector<std::filesystem::path> includeDirectories;
        std::vector<std::filesystem::path> libraryDirectories;
        std::vector<std::string> flags;
        std::string compilerPath;
        bool runProgram = false;
        bool printLLVMIR = false;
        bool printAST = false;
        bool colorOutput = true;
        bool ggdb = false;
        std::vector<std::string> runArguments;
        std::optional<std::string> fuseLd = std::nullopt;
    };

    std::string shiftarg(std::vector<std::string> &args);

    CompilerOptions parseCompilerOptions(std::vector<std::string> &argList);
}
