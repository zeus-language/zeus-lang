//
// Created by stefan on 12.09.25.
//
#include "lexer/Lexer.h"

int main() {
    lexer::lex_file("hello.zeus", "fn main() { let x: int = 42; }");
    return 0;
}
