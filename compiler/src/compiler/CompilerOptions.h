#pragma once
#include <filesystem>
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
        std::string compilerPath;
        bool runProgram = false;
        bool printLLVMIR = false;
        bool printAST = false;
        bool lsp = false;
        bool colorOutput = true;
        std::vector<std::string> runArguments;
    };

    std::string shiftarg(std::vector<std::string> &args);

    CompilerOptions parseCompilerOptions(std::vector<std::string> &argList);
}
