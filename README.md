# rlox

Rust implementations of the jlox and clox interpreters from the book [Crafting Interpreters](https://craftinginterpreters.com/).

> jlox is a tree-walk interpreter written in Java and clox is a bytecode interpreter written in C.
> You can see my clox implementation [here](https://github.com/Emivvvvv/clox)

# rjlox roadmap (safe)
Rust implementation of the jlox interpreter from the second chapter of "Crafting Interpreters".

|        Chapter         | Status |
|:----------------------:|:------:|
|        Scanning        |   ✅    |
|   Representing Code    |   ✅    |
|  Parsing Expressions   |   ✅    |
| Evaluating Expressions |   ✅    |
|  Statements and State  |   ✅    |
|      Control Flow      |   ✅    |
|       Functions        |   ✅    |
| Resolving and Binding  |   ✅    |
|        Classes         |   ✅    |
|      Inheritance       |   ✅    |

# rclox roadmap (unsafe)
Rust implementation of the clox interpreter from the third chapter of "Crafting Interpreters".
The foundation of rclox was inspired by this [implementation](https://github.com/jeschkies/lox-rs/blob/master/bytecode/).

|         Chapter          | Status |
|:------------------------:|:------:|
|    Chunks of Bytecode    |   ⏳    |
|    A Virtual Machine     |   ⏳    |
|    Scanning on Demand    |   ⏳    |
|  Compiling Expressions   |   ⏳    |
|     Types of Values      |   ⏳    |
|         Strings          |   ⏳    |
|       Hash Tables        |   ⏳    |
|     Global Variables     |   ⏳    |
|     Local Variables      |   ⏳    |
|  Jumping Back and Forth  |   ⏳    |
|   Calls and Functions    |   ⏳    |
|         Closures         |   ⏳    |
|    Garbage Collection    |   ⏳    |
|  Classes and Instances   |   ⏳    |
| Methods and Initializers |   ⏳    |
|       Superclasses       |   ⏳    |
|       Optimization       |   ⏳    |

# benchmark

```lox
fun fib(n) {
  if (n < 2) return n;
  return fib(n - 1) + fib(n - 2);
}

var before = clock();
print fib(40);
var after = clock();
print after - before;
```

rjlox: 
```shell
cargo run --release -- test.rlox
    Finished `release` profile [optimized] target(s) in 0.04s
     Running `target/release/rjlox test.rlox`
102334155
168.32861804962158
```
rclox:
```shell
STILL ON DEVELOPMENT.
```
