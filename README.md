# zeus

## Compiler

The compiler is based on llvm and will generate a native binary for the target plattform.
For now only `linux-x86-64` and `win64` are supported.

# Usage

```sh
zeus examples/helloworld.zeus
```

## Executing the compiler

The compiler will generate a native executable based on the program name defined in the program unit.

```sh
zeus -c testfiles/hello.zeus
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

## Variables

```rust
fn main() : i32 {
    let x : i32 = 10;
    let y : i32 = 20;
    let z : i32 = x + y;
    println("The sum value is: ");
    println(z);
    return 0;
} 
``` 

## Control Flow

```rust
fn main() : i32 {
    let mut i : i32 = 0;
    while i < 10 {
        println(i);
        i = i + 1;
    }
    return 0;   
}
```
