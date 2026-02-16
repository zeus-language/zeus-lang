#include "compiler/Compiler.h"
#include <algorithm>
#include <fstream>
#include <gtest/gtest.h>
#include <iostream>
#include <string>
#include <utility>

#include "os/command.h"

using namespace std::literals;

class CompilerTest : public testing::TestWithParam<std::string> {
protected:
    modules::ModuleCache moduleCache = modules::ModuleCache(true);
public:
    static void SetUpTestSuite() {
    }
};

class CompilerIOTest : public testing::TestWithParam<std::string> {
protected:
    modules::ModuleCache moduleCache = modules::ModuleCache(true);
public:
    static void SetUpTestSuite() {
    }
};

class ProjectEulerTest : public testing::TestWithParam<std::string> {
protected:
    modules::ModuleCache moduleCache = modules::ModuleCache(true);
public:
    static void SetUpTestSuite() {
    }
};


class CompilerTestError : public testing::TestWithParam<std::string> {
protected:
    modules::ModuleCache moduleCache = modules::ModuleCache(true);
public:
    static void SetUpTestSuite() {
    }
};

class WriteToStdErrTest : public testing::TestWithParam<std::string> {
protected:
    modules::ModuleCache moduleCache = modules::ModuleCache(true);
public:
    static void SetUpTestSuite() {
    }
};

TEST_P(CompilerTest, TestNoError) {
    // Inside a test, access the test parameter with the GetParam() method
    // of the TestWithParam<T> class:
    std::filesystem::path base_path = "testfiles";
    auto name = GetParam();
    std::filesystem::path input_path = base_path / (name + ".zeus");
    std::filesystem::path output_path = base_path / (name + ".txt");
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
    compiler::parse_and_compile(options,moduleCache, input_path, erstream, ostream);

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

TEST_P(CompilerIOTest, TestReadFileNoError) {
    // Inside a test, access the test parameter with the GetParam() method
    // of the TestWithParam<T> class:
    std::filesystem::path base_path = "testfiles";
    auto name = GetParam();
    std::filesystem::path input_path = base_path / (name + ".zeus");
    std::filesystem::path output_path = base_path / (name + ".txt");
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
    options.runArguments.push_back(output_path.string());
    options.buildMode = compiler::BuildMode::Debug;
    options.outputDirectory = std::filesystem::current_path();

    compiler::parse_and_compile(options,moduleCache, input_path, erstream, ostream);


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

TEST_P(ProjectEulerTest, TestNoError) {
    // Inside a test, access the test parameter with the GetParam() method
    // of the TestWithParam<T> class:
    std::filesystem::path base_path = "projecteuler";
    const auto name = GetParam();
    std::filesystem::path input_path = base_path / (name + ".zeus");
    std::filesystem::path output_path = base_path / (name + ".txt");
    ASSERT_TRUE(std::filesystem::exists(input_path));
    ASSERT_TRUE(std::filesystem::exists(output_path));
    std::stringstream ostream;
    std::stringstream erstream;
    compiler::CompilerOptions options;
    options.stdlibDirectories.emplace_back("stdlib");

    options.buildMode = compiler::BuildMode::Release;

    options.runProgram = true;
    options.outputDirectory = std::filesystem::current_path();
    compiler::parse_and_compile(options,moduleCache, input_path, erstream, ostream);

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
    std::cout << "current path" << std::filesystem::current_path();
    std::cout << "expected: " << expected;
    std::cout << ostream.str() << "\n";
    ASSERT_EQ(erstream.str(), "");
    ASSERT_EQ(result, expected);
    ASSERT_GT(ostream.str().size(), 0);
}

//
TEST_P(CompilerTestError, CompilerTestWithError) {
    // Inside a test, access the test parameter with the GetParam() method
    // of the TestWithParam<T> class:
    std::filesystem::path base_path = "errortests";
    const auto name = GetParam();
    std::filesystem::path input_path = base_path / (name + ".zeus");
    std::filesystem::path output_path = base_path / (name + ".txt");

    ASSERT_TRUE(std::filesystem::exists(input_path));
    ASSERT_TRUE(std::filesystem::exists(output_path));
    std::stringstream ostream;
    std::stringstream erstream;
    compiler::CompilerOptions options;
    options.stdlibDirectories.emplace_back("stdlib");
    options.colorOutput = false;
    compiler::parse_and_compile(options,moduleCache, input_path, erstream, ostream);

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
    std::string placeholder = "FILENAME";
    while (expected.find(placeholder) != std::string::npos)
        expected = expected.replace(expected.find(placeholder), placeholder.size(), input_path.string());
    std::cout << "expected: " << expected;
    std::cout << ostream.str() << "\n";
    ASSERT_EQ(erstream.str(), expected);
    ASSERT_EQ(ostream.str(), "");
}

//
// TEST_P(WriteToStdErrTest, WriteToStdErrTest) {
//     // Inside a test, access the test parameter with the GetParam() method
//     // of the TestWithParam<T> class:
//     std::filesystem::path base_path = "testfiles";
//     base_path /= "stderr"s;
//     auto name = GetParam();
//     std::filesystem::path input_path = base_path / (name + ".zeus");
//     std::filesystem::path output_path = base_path / (name + ".txt");
//     std::cerr << "current path" << std::filesystem::current_path() << "\n";;
//
//     if (!std::filesystem::exists(input_path))
//         std::cerr << "absolute input path: " << std::filesystem::absolute(input_path) << "\n";;
//     if (!std::filesystem::exists(output_path))
//         std::cerr << "absolute input path: " << std::filesystem::absolute(output_path) << "\n";;
//     ASSERT_TRUE(std::filesystem::exists(input_path));
//     ASSERT_TRUE(std::filesystem::exists(output_path));
//     std::stringstream ostream;
//     std::stringstream erstream;
//     compiler::CompilerOptions options;
//     options.stdlibDirectories.emplace_back("rtl");
//
//     options.runProgram = true;
//     options.buildMode = compiler::BuildMode::Release;
//     options.outputDirectory = std::filesystem::current_path();
//     compiler::parse_and_compile(options, input_path, erstream, ostream);
//
//     std::ifstream file;
//     std::istringstream is;
//     std::string s;
//     std::string group;
//
//     file.open(output_path, std::ios::in);
//
//     if (!file.is_open()) {
//         std::cerr << input_path.string() << "\n";
//         std::cerr << std::filesystem::absolute(input_path) << "\n";
//         FAIL();
//     }
//     std::stringstream buffer;
//     buffer << file.rdbuf();
//     auto expected = buffer.str();
//     std::string result = erstream.str();
//
//     result.erase(std::ranges::remove(result, '\r').begin(), result.end());
//     if (result != expected) {
//         std::cout << "expected: " << expected;
//         std::cout << result << "\n";
//     }
//
//
//     ASSERT_EQ(ostream.str(), "");
//     ASSERT_EQ(result, expected);
// }

INSTANTIATE_TEST_SUITE_P(CompilerTestNoError, CompilerTest,
                         testing::Values("helloworld","math","functions","conditions","whileloop","forloop","arraytest",
                             "usemath","chararray","mixedtypes","structtest","nestedstructs","uselibc","nestedloops",
                             "strings","matchint","simpleenums","structmethod","arraylist","functionoverloading",
                             "functionpointer","externannotation","stringslice", "convert2string","convertfromstring","operator_overloading","comparestring","stringinterpolation"));
INSTANTIATE_TEST_SUITE_P(TestReadFileNoError, CompilerIOTest,
                         testing::Values("readfile"));


INSTANTIATE_TEST_SUITE_P(CompilerTestWithError, CompilerTestError,
                         testing::Values("returntype","constmodification","typeerror_operator","unclosed_string","temp_reference"));
//
INSTANTIATE_TEST_SUITE_P(ProjectEuler, ProjectEulerTest,
                         testing::Values("problem1","problem2", "problem3"));
//
// INSTANTIATE_TEST_SUITE_P(WriteToStdErrTest, WriteToStdErrTest, testing::Values());
