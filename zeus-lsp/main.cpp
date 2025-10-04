//
// Created by stefan on 12.09.25.
//
#include <iostream>

#include "lexer/Lexer.h"
#include "lsp/LanguageServer.h"

void printHelp(const std::string &program) {
    std::cout << "Usage: " + program + " [options] \n";
    std::cout << "Options:\n";
    std::cout << "  --stdlib\t\t\tsets the path for the standard li\n";
    std::cout << "  --help\t\tOutputs the program help\n";
    std::cout << "  --version\t\tPrints the current version of the compiler\n";
}

int main(int args, char **argv) {
    using namespace std::literals;
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
    lsp::LspOptions options = lsp::parseLspOptions(argList);
#ifdef ZEUS_PATH
    options.stdlibDirectories.emplace_back(ZEUS_PATH + "/stdlib"s);
#endif

    if (const char *env_p = std::getenv("ZEUS_PATH")) {
        options.stdlibDirectories.emplace_back(env_p + "/stdlib"s);
    } else if (options.stdlibDirectories.empty()) {
        options.stdlibDirectories.push_back(std::filesystem::current_path() / "stdlib");
    }

    LanguageServer language_server(options);
    language_server.handleRequest();

    return 0;
}
