#include <filesystem>
#include <fstream>
#include <iostream>
#include "lexer/Lexer.h"
#include "magic_enum/magic_enum.hpp"
#include "parser/Parser.h"
#include "../compiler/Compiler.h"

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

using namespace std::literals;

void printHelp(const std::string &program) {
  std::cout << "Usage: " + program + " [options] file...\n";
  std::cout << "Options:\n";
  std::cout << "  --run\t\t\tRuns the compiled program\n";
  std::cout << "  --debug\t\tCreates a debug build\n";
  std::cout << "  --release\t\tCreates a release build\n";
  std::cout << "  --rtl\t\t\tsets the path for the rtl (run time library)\n";
  std::cout << "  --output\t\tsets the output / build directory\n";
  std::cout << "  --llvm-ir\t\tOutputs the LLVM-IR to the standard error output\n";
  std::cout << "  --help\t\tOutputs the program help\n";
  std::cout << "  --version\t\tPrints the current version of the compiler\n";
  std::cout << "  --lsp\t\t\tStarts the compiler in the language server mode\n";
}

int main(int args, char **argv) {
  std::vector<std::string> argList;
  for (int i = 0; i < args; i++) {
    argList.emplace_back(argv[i]);
  }

  auto program = argList[0];

  if (argList.size() == 2) {
    if (argList[1] == "--version"sv || argList[1] == "-v"sv) {
      // std::cout << "Version: " << WIRTHX_VERSION_MAJOR << "." << WIRTHX_VERSION_MINOR << "."
      //     << WIRTHX_VERSION_PATCH << "\n";
      return 0;
    }
    if (argList[1] == "--help"sv || argList[1] == "-h"sv) {
      printHelp(program);
      return 0;
    }
  }
  compiler::CompilerOptions options = compiler::parseCompilerOptions(argList);

  std::filesystem::path programPath(program);
  options.stdlibDirectories.push_back(programPath.parent_path() / "rtl");
  if (argList.empty()) {
    std::cerr << "input file is missing\n";
    printHelp(program);
    return 1;
  }
  std::filesystem::path file_path(argList[0]);
  if (!std::filesystem::exists(file_path)) {
    std::cerr << "the first argument is not a valid input file\n";
    return 1;
  }

  compiler::parse_and_compile(options, file_path, std::cerr, std::cout);


  return 0;
}
