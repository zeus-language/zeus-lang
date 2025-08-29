#include <filesystem>
#include <fstream>
#include <iostream>
#include "lexer/Lexer.h"
#include "magic_enum/magic_enum.hpp"
#include "parser/Parser.h"
#include "compiler/Compiler.h"

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

int main() {
  auto path = std::filesystem::current_path() / "examples" / "helloworld.zeus";
  auto content = read_file(path);
  auto tokens = lexer::lex_file(path, content.value());
  // for (const auto &token: tokens) {
  //   std::cout << "Token Type: " << std::string(
  //         magic_enum::enum_name(token.type)) << " Lexical: " << token.lexical() << " at Line: " << token.source_location
  //       .row
  //       << ", Column: " << token.source_location.col << std::endl;
  // }
  auto parse_result = parser::parse_tokens(tokens);
  for (const auto &message: parse_result.messages) {
    message.msg(std::cerr, true);
  }
  compiler::CompilerOptions options;
  options.outputDirectory = std::filesystem::temp_directory_path();
  options.colorOutput = true;
  options.printLLVMIR = true;
  options.runProgram = true;
  compiler::compile(options, "MyModule", parse_result.nodes);


  return 0;
}
