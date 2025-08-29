# zeus

## Compiler

The compiler is based on llvm and will generate a native binary for the target plattform.
For now only `linux-x86-64` and `win64` are supported.

# Usage

```sh
wirthx examples/helloworld.zeus
```

## Executing the compiler

The compiler will generate a native executable based on the program name defined in the program unit.

```sh
wirthx -c testfiles/hello.pas
```

# Examples

## Hello World

```rust
fn main() : i32 {
    println("Hello, World!");
    return 0;
}
```

## Functions

```rust
fn min(a : i32, b : i32) : i32 {
    if a < b {
        return a;
    } else {
        return b;
    }
}
```
