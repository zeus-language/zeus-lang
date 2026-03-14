#include "compiler/Compiler.h"
#include <algorithm>
#include <fstream>
#include <gtest/gtest.h>
#include <iostream>
#include <string>


using namespace std::literals;
static modules::ModuleCache moduleCache = modules::ModuleCache(true);

static auto platform_tests = testing::Values("path_tests", "file_tests");
#if defined(_WIN32) || defined(_WIN64)
class PlattformWinTest : public testing::TestWithParam<std::string> {
protected:
public:
    static void SetUpTestSuite() {
    }
};
TEST_P(PlattformWinTest, Win) {
    // Inside a test, access the test parameter with the GetParam() method
    // of the TestWithParam<T> class:
    std::filesystem::path base_path = "stdlib";
    auto &name = GetParam();
    std::filesystem::path input_path = base_path / "win" / (name + ".zeus");
    std::filesystem::path output_path = base_path / "win" / (name + ".txt");
    std::cerr << "current path" << std::filesystem::current_path();

    if (!std::filesystem::exists(input_path))
        std::cerr << "absolute input path: " << std::filesystem::absolute(input_path);
    if (!std::filesystem::exists(output_path))
        std::cerr << "absolute input path: " << std::filesystem::absolute(output_path);
    ASSERT_TRUE(std::filesystem::exists(input_path));
    ASSERT_TRUE(std::filesystem::exists(output_path));
    std::stringstream ostream;
    std::stringstream erstream;
    compiler::CompilerOptions options;
    options.stdlibDirectories.emplace_back("stdlib");

    options.runProgram = true;
    options.buildMode = compiler::BuildMode::Debug;
    options.outputDirectory = std::filesystem::current_path();
    env::Environment env = env::buildEnvironment();
    compiler::parse_and_compile(options, env, moduleCache, input_path, erstream, ostream);

    std::ifstream file;
    std::istringstream is;
    std::string s;
    std::string group;

    file.open(output_path, std::ios::in);

    if (!file.is_open()) {
        std::cerr << input_path.string() << "\n";
        std::cerr << std::filesystem::absolute(input_path);
        FAIL();
    }
    std::stringstream buffer;
    buffer << file.rdbuf();
    auto expected = buffer.str();
    std::string result = ostream.str();

    result.erase(std::ranges::remove(result, '\r').begin(), result.end());


    ASSERT_EQ(erstream.str(), "");
    ASSERT_EQ(result, expected);
}
INSTANTIATE_TEST_SUITE_P(Win, PlattformWinTest,
                         platform_tests);
#elif defined(__linux__)
class PlattformLinTest : public testing::TestWithParam<std::string> {
protected:
public:
    static void SetUpTestSuite() {
    }
};

TEST_P(PlattformLinTest, Linux) {
    // Inside a test, access the test parameter with the GetParam() method
    // of the TestWithParam<T> class:
    std::filesystem::path base_path = "stdlib";
    auto &name = GetParam();
    std::filesystem::path input_path = base_path / "linux" / (name + ".zeus");
    std::filesystem::path output_path = base_path / "linux" / (name + ".txt");
    std::cerr << "current path: " << std::filesystem::current_path() << "\n";

    if (!std::filesystem::exists(input_path))
        std::cerr << "absolute input path: " << std::filesystem::absolute(input_path) << "\n";
    if (!std::filesystem::exists(output_path))
        std::cerr << "absolute input path: " << std::filesystem::absolute(output_path) << "\n";
    ASSERT_TRUE(std::filesystem::exists(input_path));
    ASSERT_TRUE(std::filesystem::exists(output_path));
    std::stringstream ostream;
    std::stringstream erstream;
    compiler::CompilerOptions options;
    options.stdlibDirectories.emplace_back("stdlib");

    options.runProgram = true;
    options.buildMode = compiler::BuildMode::Debug;
    options.outputDirectory = std::filesystem::current_path();
    env::Environment env = env::buildEnvironment();
    compiler::parse_and_compile(options, env, moduleCache, input_path, erstream, ostream);

    std::ifstream file;
    std::istringstream is;
    std::string s;
    std::string group;

    file.open(output_path, std::ios::in);

    if (!file.is_open()) {
        std::cerr << input_path.string() << "\n";
        std::cerr << std::filesystem::absolute(input_path);
        FAIL();
    }
    std::stringstream buffer;
    buffer << file.rdbuf();
    auto expected = buffer.str();
    std::string result = ostream.str();

    result.erase(std::ranges::remove(result, '\r').begin(), result.end());


    ASSERT_EQ(erstream.str(), "");
    ASSERT_EQ(result, expected);
}


INSTANTIATE_TEST_SUITE_P(Linux, PlattformLinTest, platform_tests
);
#else
#error("unsupported platform")
#endif
