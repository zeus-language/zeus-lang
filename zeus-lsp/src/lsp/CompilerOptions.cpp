#include "CompilerOptions.h"
#include <filesystem>

namespace lsp {
    std::string shiftarg(std::vector<std::string> &args) {
        auto result = args.front();
        args.erase(args.begin());
        return result;
    }


    LspOptions parseLspOptions(std::vector<std::string> &argList) {
        using namespace std::literals;
        LspOptions options;
        options.compilerPath = shiftarg(argList);

        while (!argList.empty()) {
            auto arg = shiftarg(argList);
            if (arg == "--stdlib"sv) {
                options.stdlibDirectories.emplace(options.stdlibDirectories.begin(), shiftarg(argList));
            } else {
                argList.push_back(arg);
                break;
            }
        }
#ifdef ZEUS_PATH
        options.stdlibDirectories.emplace_back(ZEUS_PATH + "/stdlib"s);
#endif

        if (const char *env_p = std::getenv("ZEUS_PATH")) {
            options.stdlibDirectories.emplace_back(env_p + "/stdlib"s);
        } else if (options.stdlibDirectories.empty()) {
            options.stdlibDirectories.push_back(std::filesystem::current_path() / "stdlib");
        }

        return options;
    }
}
